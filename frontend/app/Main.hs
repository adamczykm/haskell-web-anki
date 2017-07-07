{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Reflex
import Reflex.Dom
import Data.Map (Map, fromList)
import qualified Data.Map as M
import Data.Text (Text)
import Data.Char (toLower)
import qualified Data.Text as T
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Monoid
import Control.Monad (sequence, void, liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import Text.Read (readMaybe)
import NonEmpty

import Domain
import ReflexDomApp
import Services
import ApiClient
import Debug
import WorkflowUtils (WorkflowWidget(..), widgetGraphWorkflow)
import Keyboard
import ReflexFix

------------------------ MAIN -----------------------------------------

main :: IO ()
main = runReflexDomApp testServiceApp productionServices

testServiceApp :: ReflexDomApp()
testServiceApp = ReflexDomApp mainLayout


--------------------- MAIN LAYOUT --------------------------------------

mainLayout :: MonadWidget t m => ReflexServiceFun2 'ResourceService 'LoggingService m ()
mainLayout = nestDivClasses ["window", "window-content", "pane-group"] $
  void $ workflow ankiSelectionWorkflowWidget2

------------------------ ANKI WIDGET -----------------------------------------

maybeAttach :: a -> Maybe b -> Maybe (a, b)
maybeAttach _ Nothing  = Nothing
maybeAttach a (Just b) = Just (a,b)

buildAnkiDisplayWorkflow :: MonadWidget t m => AnkiProgress -> Workflow t m (Event t (Maybe (AnkiStep, Answer)))
buildAnkiDisplayWorkflow = ankiSeriesProcessingWorkflow final ankiAnswerWidget
  where
    final _ = do
      (e, _) <- divClass' "anki-widget-main-text" $ text "Koniec"
      return (domEvent Click e)

    ankiSeriesProcessingWorkflow :: MonadWidget t m =>
        (AnkiProgress -> m (Event t ())) ->
        -- ^ Widget for finalization (all list element processed)
        (AnkiProgress -> m (Maybe (Event t Answer))) ->
        -- ^ main widget for processing list element
        AnkiProgress ->
        -- ^ list of elements to be processed
        Workflow t m (Event t (Maybe (AnkiStep, Answer)))
        -- ^ resulting worklow
    ankiSeriesProcessingWorkflow finalW w' (MkAnkiProgress a astep cfg) = go finalWNothing (constant astep) w'
      where

        ankiProgress :: AnkiStep -> AnkiProgress
        ankiProgress as = MkAnkiProgress a as cfg

        finalWNothing = Workflow $ do
          e <- finalW $ ankiProgress astep
          return (const Nothing <$> e, never)

        --newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }
        go :: MonadWidget t m =>
              Workflow t m (Event t (Maybe (AnkiStep, Answer))) ->
              Behavior t AnkiStep ->
              (AnkiProgress -> m (Maybe (Event t Answer))) ->
              Workflow t m (Event t (Maybe (AnkiStep, Answer)))

        go wf asB w = Workflow $ do
          as' <- sample asB
          mAns <- w (ankiProgress as')
          case mAns of
            Nothing  -> unWorkflow wf
            Just ansE -> do
              let aE = fmap (ankiStepWithAnswer cfg as' . const) ansE
              newAsB <- switcher asB (constant . fst <$> aE)
              return (uncurry maybeAttach <$> aE, const (go wf newAsB w) <$> aE)

ankiAnswerWidget :: MonadWidget t m => AnkiProgress -> m (Maybe (Event t Answer))
ankiAnswerWidget (MkAnkiProgress _ (ASDone _) _)  = return Nothing
ankiAnswerWidget (MkAnkiProgress _ (AStep q _) _)
  = (Just . snd <$>) . ankiWidget' . qsQuestion . neHead $ q


ankiWidget' :: MonadWidget t m => Question -> m (El t, Event t Answer)
ankiWidget' q = do
  -- divClass' "anki-widget" $ do
  --   ans <- buttonClass "button right" "Good"
  --   return $ const Good <$> ans
  rec (e, viewer) <- divClass' "anki-widget" $ do
        de <- widgetHoldHelper (state2Widget q) Prompting wantsAnswer
        return (switch $ current de)
      kbdAccept <- anyKeysDownQuery [13,37,39]
      let wantsAnswer = const Answering <$> leftmost [void kbdAccept,
                                                      domEvent Click e]
      -- ^ Event after which the widget will get into the Answering state.

  return (e, viewer)
    where

      state2Widget :: MonadWidget t m => Question -> AnswerState -> m (Event t Answer)
      state2Widget q' Prompting = promptViewer q'
      state2Widget q' Answering = answViewer q'

      promptViewer q'' = divClass "question-container" $ do
        _ <- spanClass' "question-text" $ text  (T.unpack $ question q'')
        return never

      answViewer q' = do
        _ <- divClass "ans-img-container" $ imgClass' "ans-img" (imgSrcForQuestion q')
        (goodE, badE) <- divClass "ans-buttons-container" $ do
          g <- buttonClass "button big-btn btn-good" "Good"
          gk <- anyKeysDownQuery [39,13]
          b <- buttonClass "button big-btn btn-bad " "Bad"
          bk <- keyDownQuery 37
          performEvent_ $ liftIO . print <$> bk
          return (leftmost [g, void gk],
                  leftmost [b, void bk])

        return $ leftmost [ const Good <$> goodE
                          , const Bad <$> badE]


ankiWidget :: MonadWidget t m => Question -> m (Event t Answer)
ankiWidget = (snd <$>) . ankiWidget'


-------------------------------- RIGHT PANE LAYOUT --------------------------------------

data ListElem a = ListElem { _listElemValue        :: a
                           , _listElemName         :: Text
                           , _listContentTextRight :: Text} deriving (Show, Eq, Functor)


data AnswerState = Prompting | Answering
                   deriving (Eq,Show,Ord,Enum)


collectionToListElem :: CollectionInfo -> ListElem CollectionInfo
collectionToListElem ci@MkCollectionInfo{..} = ListElem ci
                                            (T.pack _ciName)
                                            (T.pack $ show _ciQuestionCount ++ " pytań")

listLeftMostClick :: MonadWidget t m => m [(El t, a)] -> m (Event t a)
listLeftMostClick list' = leftmost <$> fmap (map clickHelp) list'
  where
    clickHelp :: Reflex t => (El t, a) -> Event t a
    clickHelp (e, a) = const a <$> domEvent Click e

listWithElems :: MonadWidget t m => [ListElem a] -> m [(El t, a)]
listWithElems = listGroup . mapM createLiWidget
  where
    listGroup = elClass "ul" "anki-list-group"
    createLiWidget (ListElem v name right) =
      elClass' "li" "anki-list-group-item hvr-backward" $ do
      _ <- divClass "media-body" $ do
        el "strong" $ text (T.unpack name)
        elClass "span" "inline-block-float-right" $ text (T.unpack right)
      return v

--------------------- Config widget ----------------------------

filterDynamicChanges :: MonadWidget t m => (a-> Maybe b) -> b -> Dynamic t a -> m (Dynamic t b)
filterDynamicChanges p initial dynamic = holdDyn initial (fmapMaybe p $ updated dynamic)

integerInput :: MonadWidget t m => Integer -> m (Dynamic t String)
integerInput initial = _textInput_value <$> divClass "number-input" numInp
  where
    numInp = textInput $ def & textInputConfig_inputType .~ "number"
      & textInputConfig_initialValue .~ show initial


checkList :: MonadWidget t m => (Bool -> a -> m (Dynamic t Bool)) -> [(Bool, a)] -> m (Dynamic t [a])
checkList checkWidget items = do
  -- dynamic map of bindings (a -> Bool)
  dynMap <- joinDynThroughMap . constDyn . fromList . zip indices <$> mapM (uncurry checkWidget) items
  -- filter only checked values indices
  checkdIdx <- mapDyn (M.keys . M.filter id) dynMap
  -- select checked items
  mapDyn (\idxx -> fastFilterIdx [] idxx (zip indices (snd (unzip items)))) checkdIdx
  where
    indices :: [Int]
    indices = [0..]
    -- lists must be sorted
    fastFilterIdx acc []              _  = reverse acc
    fastFilterIdx acc _               [] = reverse acc
    fastFilterIdx acc allIxx@(ix:ixx) allIvals@((iv,val):ivals) = case compare ix iv of
      LT -> fastFilterIdx acc ixx allIvals
      EQ -> fastFilterIdx (val:acc) ixx ivals
      GT -> fastFilterIdx acc allIxx ivals

-------------- new ui

data AnkiSelectionState = AllLoaded AnkiProgress
                        | Loading Anki
                        | NotLoaded
                        deriving (Show)


ankiSelectionWidget :: MonadWidget t m => WorkflowWidget t m String AnkiSelectionState
ankiSelectionWidget = MkWorkflowWidget $ \ankiState -> do
  -- get available collections from server every n sec.
  colls <- (fmap.fmap) (sortBy (comparing (map toLower . _ciName))) <$> (getPostBuild >>= getCollectionsInfo)
  -- colls <- liftM2 (<>) getPostBuild (getTickCounter 10) >>= getAnkiNamesList

  -- construct dynamic list with available collections
  -- and stores event of clicking one of its elements
  ankiLoaded <- getAnki =<< divClass "collections-list " (switchPromptlyDyn <$>
                                                            widgetHold (listLeftMostClick $ listWithElems [])
                                                                       (constructCollectionList <$> colls))

  -- if anki is loaded proceed to next workflowwidget, and update state
  let flowAction = ffor ankiLoaded $ \al -> (Loading al, Just "ankiConfigurationWidget")

  return (ankiState, flowAction)
  where
    constructCollectionList :: MonadWidget t m => Maybe [CollectionInfo] -> m (Event t CollectionInfo)
    constructCollectionList Nothing = listLeftMostClick $ listWithElems []
    constructCollectionList (Just xs ) = listLeftMostClick . listWithElems . map collectionToListElem $ xs


ankiConfigurationWidget :: MonadWidget t m => WorkflowWidget t m String AnkiSelectionState
ankiConfigurationWidget = undefined
ankiProgressVisualisationWidget :: MonadWidget t m => WorkflowWidget t m String AnkiSelectionState
ankiProgressVisualisationWidget = MkWorkflowWidget $ \case
  ass@(AllLoaded ap) -> undefined
  ass                -> do
    text "wszystko smiga"
    return (ass, never)

ankiSelectionWorkflowWidget :: MonadWidget t m => Workflow t m AnkiSelectionState
ankiSelectionWorkflowWidget = widgetGraphWorkflow workflowWidgets NotLoaded "ankiSelectionWidget"
  where
    workflowWidgets :: MonadWidget t m => Map String (WorkflowWidget t m String AnkiSelectionState)
    workflowWidgets = M.fromList [ ("ankiSelectionWidget", ankiSelectionWidget)
                                 , ("ankiConfigurationWidget", ankiConfigurationWidget)
                                 , ("ankiProgressVisualisationWidget", ankiProgressVisualisationWidget)]


ankiSelectionWorkflowWidget2 :: MonadWidget t m => Workflow t m AnkiSelectionState
ankiSelectionWorkflowWidget2 = widgetGraphWorkflow workflowWidgets NotLoaded "collectionSelectionWidget"
  where
    workflowWidgets :: MonadWidget t m => Map String (WorkflowWidget t m String AnkiSelectionState)
    workflowWidgets = M.fromList [ ("collectionSelectionWidget", collectionSelectionWidget)
                                 , ("mainAnkiWidget", mainAnkiWidget)]


mainAnkiWidget :: MonadWidget t m => WorkflowWidget t m String AnkiSelectionState
mainAnkiWidget = MkWorkflowWidget $ \case
  ass@(AllLoaded ap@(MkAnkiProgress anki _ cfg)) -> do
    -- anki answering widget
    ankiAnsweredE <- switchPromptlyDyn <$> workflow (buildAnkiDisplayWorkflow ap)
    ankiAnswerDyn <- holdDyn Nothing ankiAnsweredE

    -- apply new step to current progress
    newAss <- forDyn ankiAnswerDyn $ \case
          Nothing            -> ass
          Just (astep, _) -> AllLoaded (MkAnkiProgress anki astep cfg)

    -- back button that "saves" current progress
    backButtonPressed <- buttonClass "btn-back btn-absolute" "Back"
    let backAction = ffor (tagPromptlyDyn newAss backButtonPressed) (\na -> (na, Just "collectionSelectionWidget"))

    return (ass, leftmost [backAction, never])

  ass -> do
    backButtonPressed <- buttonClass "btn-back" "Back"
    let backAction = ffor backButtonPressed $ const (ass, Just "collectionSelectionWidget")
    text "wszystko smiga"
    return (ass, leftmost [backAction, never])


ankiNotLoadedWidget :: MonadWidget t m => a -> m a
ankiNotLoadedWidget d = const d <$> divClass' "anki-widget-main-text" (text "Anki not loaded.")

collectionSelectionWidget :: MonadWidget t m => WorkflowWidget t m String AnkiSelectionState
collectionSelectionWidget = MkWorkflowWidget $ \ankiState -> do

  -- currently selected anki
  let (anki, ankiConfig) = case ankiState of
        AllLoaded (MkAnkiProgress a _ ac) -> (Just a, ac)
        _                                 -> (Nothing, defaultAnkiConfig)


  -- anki selected from the list, or continued if ongoing
  (continueButtonPressed, ankiSelected) <- divClass "darker-back anki-pane-sm sidebar padded" $ do

    -- continue anki button
    continueButtonPressed' <- case ankiState of
          ass@(AllLoaded _) -> (fmap.fmap) (const (ass, Just ("mainAnkiWidget" :: String))) (buttonClass "btn-start" "Continue")
          ass               -> (fmap.fmap) (const (ass, Just ("collectionSelectionWidget" :: String))) (return never)

    -- get available collections from server every n sec.
    colls <- (fmap.fmap) (sortBy (comparing (map toLower . _ciName))) <$> (getPostBuild >>= getCollectionsInfo)
    -- colls <- liftM2 (<>) getPostBuild (getTickCounter 10) >>= getAnkiNamesList

    -- construct dynamic list with available collections
    -- and stores event of clicking one of its elements
    ankiSelected' <- getAnki =<< divClass "collections-list " (switchPromptlyDyn <$>
                                                widgetHold (listLeftMostClick $ listWithElems [])
                                                            (constructCollectionList <$> colls))

    return (continueButtonPressed', ankiSelected')

  -- widget for configuring and starting anki, it will apply currently loaded config if anki is selected
  ankiConfiguredAndStarted <- divClass "pane anki-pane" $ configureAndStartAnkiWidget (anki, ankiConfig) ((\as -> (Just as, defaultAnkiConfig)) <$> ankiSelected)
  let flowAction = ffor ankiConfiguredAndStarted $ \ap -> (AllLoaded ap, Just "mainAnkiWidget")

  return (ankiState, leftmost [continueButtonPressed, flowAction])

  where

    configureAndStartAnkiWidget :: MonadWidget t m =>
                                   (Maybe Anki, AnkiConfig) -> Event t (Maybe Anki, AnkiConfig)
                                -> m (Event t AnkiProgress)

    configureAndStartAnkiWidget start changingE = switchPromptlyDyn <$>
                                                  widgetHoldHelper createConfigurationWidget start changingE
      where
        createConfigurationWidget (Nothing,   _)          = ankiNotLoadedWidget never
        createConfigurationWidget (Just anki, ankiConfig) = do
          --  configure anki and prepare started anki
          ankiConfigDyn <- ankiConfigurationWidget2 anki ankiConfig >>= mapDyn (startAnki anki)

          -- start anki button
          startButtonPressed <- buttonClass "btn-start" "Start"
          startKeyboard <- anyKeysDownQuery [13,39]
          let startEvent = leftmost [ startButtonPressed,
                                      void startKeyboard]

          return $ tagPromptlyDyn ankiConfigDyn startEvent

    constructCollectionList :: MonadWidget t m => Maybe [CollectionInfo] -> m (Event t CollectionInfo)
    constructCollectionList Nothing = listLeftMostClick $ listWithElems []
    constructCollectionList (Just xs ) = listLeftMostClick . listWithElems . map collectionToListElem $ xs

    ankiConfigurationWidget2 :: MonadWidget t m => Anki -> AnkiConfig -> m (Dynamic t AnkiConfig)
    ankiConfigurationWidget2 anki ac = do
        -- config data inputs
        comboLen <- comboInputWidget ac
        queSel <- questionSelectorWidget anki ac
        combineDyn MkAnkiConfig comboLen queSel

      where

        comboInputWidget cfg = divClass "combo-div" $ text "Combo" *> (integerInput (_comboLength cfg) >>= filterDynamicChanges readMaybe comboLength)

        questionSelectorWidget :: MonadWidget t m => Anki -> AnkiConfig -> m (Dynamic t QuestionSelector)
        questionSelectorWidget (Anki _ qs) (MkAnkiConfig _ qsm) =
          let questionSelectionState = case qsm of
                QsAll               -> zip (repeat True) qs
                QsSelected selected -> map (\q -> (q `elem` selected, q)) qs
          in
          divClass "question-selector" (checkList questionSelectCheckbox questionSelectionState) >>= mapDyn QsSelected

        questionSelectCheckbox checked (Question _ _ qText) = divClass "question-checkbox" ((_checkbox_value <$> checkbox checked def) <* text (T.unpack qText))

--------------------- UI AUXILLIARY ----------------------------------

nestR :: Foldable t => t (a -> a) -> a -> a
nestR = flip $ foldr ($)

nestDivClasses :: MonadWidget t m => [String] -> m a -> m a
nestDivClasses classes = nestR (map divClass classes)

divClass' :: MonadWidget t m => String -> m a -> m (El t, a)
divClass' cls = elAttr' "div" $ "class" =: cls

buttonClass :: MonadWidget t m => String -> String -> m (Event t ())
buttonClass cls s = do
  (e, _) <- elAttr' "button" (M.fromList [("type", "button"),("class",cls)]) $ text s
  return $ domEvent Click e

spanClass' :: MonadWidget t m => String -> m a -> m (El t, a)
spanClass' cls = elAttr' "span" $ "class" =: cls

imgClass' :: MonadWidget t m => String -> String -> m (El t, ())
imgClass' cls src = elAttr' "img" (fromList [("class", cls), ("src", src)]) $ return ()

elClass' :: MonadWidget t m => String -> String -> m a -> m (El t, a)
elClass' elementTag c = elWith' elementTag $ def & attributes .~ "class" =: c

-- widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
widgetHoldHelper :: MonadWidget t m
    => (a -> m b) -> a -> Event t a
    -> m (Dynamic t b)
widgetHoldHelper f eDef e = widgetHold (f eDef) (f <$> e)


------------------- OTHER AUXILIARY -----------------------------

getTickCounter :: MonadWidget t m => Int -> m (Event t ())
getTickCounter interval = void <$> (liftIO getCurrentTime >>= tickLossy (realToFrac interval))

myFmapMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
myFmapMaybe f (Just v) = sequence . Just . f $ v
myFmapMaybe _ Nothing  = return Nothing


  -- where

  --   completionInfoWidget :: MonadWidget t m => AnkiProgress -> Event t (Maybe (AnkiStep,Answer)) -> m (Dynamic t (Int, Int))
  --   completionInfoWidget ankiLoaded maaE = divClass "completion-info-widget" $ do
  --     -- ankistate, reseted by ankiLoaded
  --     ankiDyn <- holdDyn Nothing ankiLoaded
  --     ankiStateDyn <- holdDyn Nothing (leftmost [const Nothing <$> ankiLoaded,maaE])
  --     totalandLeftDyn <- combineDyn completionInfo ankiDyn ankiStateDyn
  --     completionDynStr <- mapDyn (\(total,left) -> show (debugPrint total - debugPrint left :: Int) ++ "/" ++ show total) totalandLeftDyn
  --     text "Stan ukończenia: "
  --     dynText completionDynStr
  --     return totalandLeftDyn

  --     where
  --       completionInfo Nothing               _       = (0,0)
  --       completionInfo (Just (Anki _ allQs)) Nothing = let l = length allQs
  --                                                     in (l,l)
  --       completionInfo _ (Just (ASDone x,_))     = (length x, 0)
  --       completionInfo _ (Just (AStep (q:|qs) x,_)) = let left = length (q:qs)
  --                                                         all'  = left + length x
  --                                                     in (all', left)


  --   accuracyInfoWidget :: MonadWidget t m => Event t (Maybe Anki) -> Event t (Maybe (AnkiStep,Answer)) -> m (Dynamic t (Int, Int))
  --   accuracyInfoWidget ankiLoaded maaE = divClass "accuracy-info-widget" $ do
  --     goodRatio <- foldDyn addAnswer (0,0) (leftmost [const Nothing <$> ankiLoaded,Just <$> maaE])
  --     accuracyDynStr <- mapDyn (\(good,total) -> show good ++ "/" ++ show total) goodRatio
  --     text "Wskaźnik poprawności: "
  --     dynText accuracyDynStr
  --     return goodRatio

  --     where
  --       addAnswer Nothing _ = (0,0)
  --       addAnswer (Just Nothing) x = x
  --       addAnswer (Just (Just (_,Good))) (g,a) = (g+1,a+1)
  --       addAnswer (Just (Just (_,Bad))) (g,a) = (g,a+1)

-- -- test widget navigator

-- testLayout :: MonadWidget t m => ReflexServiceFun2 'ResourceService 'LoggingService m ()
-- testLayout = do
--   -- e <- currentBodyUnchecked
--   -- kp <- holdDyn "0" $ show <$> domEvent Keydown e
--   -- dynText kp
--   states <- workflow ankiSelectionWorkflowWidget
--   states_str <- mapDyn show states
--   dynText states_str


-- testWorkflow2_Example :: MonadWidget t m => Map String (WorkflowWidget t m String [String])
-- testWorkflow2_Example = fromList [("1", justButton "1" "2"), ("2", justButton "2" "1")]
--   where
--     justButton :: MonadWidget t m => String -> String -> WorkflowWidget t m String [String]
--     justButton name linkedWidgetName = MkWorkflowWidget $ \s -> do
--       bE <- buttonClass "button" name
--       let linkedWidgetE = fmap (const (name : s, Just linkedWidgetName)) bE
--       return (s, linkedWidgetE)

---- old

-- mainLayout :: MonadWidget t m => ReflexServiceFun2 'ResourceService 'LoggingService m ()
-- mainLayout = nestDivClasses ["window", "window-content", "pane-group"] $ mdo
--     -- e <- currentBodyUnchecked
--     -- kp <- holdDyn "0" $ show <$> (domEvent Keydown e)
--     -- dynText kp
--     -- main pane
--     maaE <- divClass "pane" $ ankiWorkflowWidget (ankiLoadedWithConfig ankiConfig startAnkiE)

--     -- left pane
--     (startButtonPressed, ankiNameSelected, ankiConfig) <- divClass "darker-back pane-sm sidebar padded" $ do

--           _ <- -- current completion and accuracy of chosen collection --
--                divClass "collection-info-container" $
--                liftM2 (,) (completionInfoWidget startAnkiE maaE)
--                           (accuracyInfoWidget startAnkiE maaE)
--                ----------------------------------------------------------

--           -- get available collections from server every n sec.
--           colls <- liftM2 (<>) getPostBuild (getTickCounter 10) >>= getAnkiNamesList

--           -- construct dynamic list with available collections
--           -- and stores event of clicking one of its elements

--           --     widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
--           ankiNameSelected <- divClass "collections-list " $ switchPromptlyDyn <$> widgetHold (listLeftMostClick $ listWithElems []) (constructCollectionList <$> colls)

--           -- config widget
--           -- ankiConfig <- ankiConfigWidget ankiLoaded

--           -- start anki button
--           startButtonPressed <- buttonClass "start-anki-button" "Start"

--           return (startButtonPressed, ankiNameSelected, ankiConfig)

--     ankiLoaded <- getAnki ankiNameSelected
--     loadedAnkiDyn <- holdDyn Nothing ankiLoaded

--     let startAnkiE = attachDynWith const loadedAnkiDyn startButtonPressed

--     return ()
--   where

--     ankiLoadedWithConfig :: Reflex t => Dynamic t AnkiConfig -> Event t (Maybe Anki) -> Event t (Maybe (Anki, AnkiConfig))
--     ankiLoadedWithConfig = attachDynWith maybeHelper
--       where
--         maybeHelper _ Nothing = Nothing
--         maybeHelper d (Just e) = Just (e, d)

--     getTickCounter :: MonadWidget t m => Int -> m (Event t ())
--     getTickCounter interval = void <$> (liftIO getCurrentTime >>= tickLossy (realToFrac interval))

--     constructCollectionList :: MonadWidget t m => Maybe [String] -> m (Event t String)
--     constructCollectionList Nothing = listLeftMostClick $ listWithElems []
--     constructCollectionList (Just xs ) = listLeftMostClick . listWithElems . map collectionToListElem $ xs


  -- printing currentyle pressed
    -- e <- currentBodyUnchecked
    -- kp <- holdDyn "0" $ show <$> (domEvent Keydown e)
    -- dynText kp
    -- main pane


-- ankiWorkflowWidget :: MonadWidget t m =>
--                       Event t (Maybe (Anki, AnkiConfig)) -> m (Event t (Maybe (AnkiStep, Answer)))
-- ankiWorkflowWidget ema = do
--   let mAp            = (fmap.fmap) (uncurry startAnki) ema
--       workflowWidget = widgetHold (questionWorkflow Nothing) (fmap questionWorkflow mAp)

--   (switchPromptlyDyn . joinDyn) <$> workflowWidget
--       where
--         questionWorkflow = workflow . buildAnkiDisplayWorkflow


-- ankiConfigWidget :: MonadWidget t m => Event t (Maybe Anki) -> m (Dynamic t AnkiConfig)
-- ankiConfigWidget maE = do

--   comboLen <- comboInputWidget
--   queSel <- questionSelectorWidget maE

--   divClass "anki-config-widget" $ combineDyn MkAnkiConfig comboLen queSel
--   where
--     comboInputWidget = text "Combo" *> (integerInput >>= filterDynamicChanges readMaybe 3)


------- question selection

-- questionSelectorWidget :: MonadWidget t m => Event t (Maybe Anki) -> m (Dynamic t QuestionSelector)
-- questionSelectorWidget dynMA = joinDyn <$> widgetHold startWidget (buildWidgetEvent dynMA)
--   where
--     startWidget :: MonadWidget t m => m (Dynamic t QuestionSelector)
--     startWidget = divClass "question-selector" $ return (constDyn QsAll)

--     checkListWidget :: MonadWidget t m => [Question] -> m (Dynamic t QuestionSelector)
--     checkListWidget qs = divClass "question-selector" (checkList questionSelectCheckbox qs) >>= mapDyn QsSelected

--     buildWidgetEvent :: MonadWidget t m => Event t (Maybe Anki) -> Event t (m (Dynamic t QuestionSelector))
--     buildWidgetEvent = fmap (\case
--                                 Nothing            -> startWidget
--                                 (Just (Anki _ qs)) -> checkListWidget qs)

--     questionSelectCheckbox (Question _ _ qText) = divClass "question-checkbox" ((_checkbox_value <$> checkbox True def) <* text (T.unpack qText))

-- ankiConfigurationWidget :: MonadWidget t m => WorkflowWidget t m String AnkiSelectionState
-- ankiConfigurationWidget = MkWorkflowWidget $ \case
--   as@(Loading anki) -> do
--     backButtonPressed <- buttonClass "btn-back" "Back"
--     backKeyboard <- anyKeysDownQuery [37]
--     let backEvent = const (as, Just backFlowWidget) <$> leftmost [ backButtonPressed,
--                                                                    void backKeyboard]

--     -- start anki button
--     startButtonPressed <- buttonClass "btn-start" "Start"
--     startKeyboard <- anyKeysDownQuery [13,39]
--     let startEvent = leftmost [ startButtonPressed,
--                                 void startKeyboard]

--     comboLen <- comboInputWidget
--     queSel <- questionSelectorWidget anki
--     -- set config
--     cfg <- divClass "anki-config-widget" $ combineDyn MkAnkiConfig comboLen queSel
--     newSelectionState <- mapDyn (AllLoaded . startAnki anki) cfg
--     worflowNext <- mapDyn (\nss -> (nss, Just nextFlowWidget)) newSelectionState
--     -- replace button event value with dynamic value
--     return (as, leftmost [backEvent, tagPromptlyDyn worflowNext startEvent])

--   as -> return (as, never)
--   where
--     backFlowWidget :: String
--     backFlowWidget = "ankiSelectionWidget"

--     nextFlowWidget :: String
--     nextFlowWidget = "ankiProgressVisualisationWidget"

--     comboInputWidget = divClass "combo-div" $ text "Combo" *> (integerInput comboLength >>= filterDynamicChanges readMaybe 3)

--     questionSelectorWidget :: MonadWidget t m => Anki -> m (Dynamic t QuestionSelector)
--     questionSelectorWidget (Anki _ qs') = checkListWidget qs'
--       where
--         checkListWidget :: MonadWidget t m => [Question] -> m (Dynamic t QuestionSelector)
--         checkListWidget qs = divClass "question-selector" (checkList questionSelectCheckbox qs) >>= mapDyn QsSelected

--         questionSelectCheckbox (Question _ _ qText) = divClass "question-checkbox" ((_checkbox_value <$> checkbox True def) <* text (T.unpack qText))


-- mainLayout :: MonadWidget t m => ReflexServiceFun2 'ResourceService 'LoggingService m ()
-- mainLayout = nestDivClasses ["window", "window-content", "pane-group"] $ mdo

--     _ <- divClass "pane anki-pane" $ ankiWorkflowWidget (Just <$> ankiLoaded)

--     -- states <- workflow ankiSelectionWorkflowWidget
--     -- left pane
--     ankiSelectionState <- divClass "darker-back anki-pane-sm sidebar padded" $ workflow ankiSelectionWorkflowWidget
--     let ankiLoaded = onAllLoaded $ updated ankiSelectionState

--     return ()
--   where

--     onAllLoaded :: Reflex t => Event t AnkiSelectionState -> Event t AnkiProgress
--     onAllLoaded e = unsafeGetProgress <$> ffilter justAllLoaded e
--       where
--         unsafeGetProgress (AllLoaded p) = p
--         unsafeGetProgress _             = error "Only call this on filtered events."

--         justAllLoaded (AllLoaded _) = True
--         justAllLoaded _             = False

-- createImgPath :: MonadWidget t m => FilePath -> m ()
-- createImgPath imgP = let (w,h) = listElemImgSize
--                       in elAttr "img"
--                         (fromList [ ("class", "img-circle media-object pull-left")
--                                   , ("src", imgP)
--                                   , ("width", show w)
--                                   , ("height", show h)])
--                         (return ())

-- listElemImgSize :: (Int,Int)
-- listElemImgSize = (32,32)

