{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Reflex
import Reflex.Dom
import qualified Data.Map as M
import Data.Map (fromList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Control.Monad (sequence, void, liftM2)
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock
import Text.Read (readMaybe)

import Domain
import ReflexDomApp
import Services
import ApiClient
import Debug

------------------------ MAIN -----------------------------------------

main :: IO ()
main = runReflexDomApp testServiceApp productionServices

testServiceApp :: ReflexDomApp()
testServiceApp = ReflexDomApp mainLayout


--------------------- MAIN LAYOUT --------------------------------------

mainLayout :: MonadWidget t m => ReflexServiceFun2 'ResourceService 'LoggingService m ()
mainLayout = nestDivClasses ["window", "window-content", "pane-group"] $ mdo

    -- main pane
    maaE <- divClass "pane" $ ankiWorkflowWidget (ankiLoadedWithConfig ankiConfig startAnkiE)

    -- left pane
    (startButtonPressed, ankiNameSelected, ankiConfig) <- divClass "pane-sm sidebar padded" $ do

          _ <- -- current completion and accuracy of chosen collection --
               divClass "collection-info-container" $
               liftM2 (,) (completionInfoWidget startAnkiE maaE)
                          (accuracyInfoWidget startAnkiE maaE)
               ----------------------------------------------------------

          -- get available collections from server every n sec.
          colls <- liftM2 (<>) getPostBuild (getTickCounter 10) >>= getAnkiNamesList

          -- construct dynamic list with available collections
          -- and stores event of clicking one of its elements

          --     widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
          ankiNameSelected <- divClass "collections-list " $ switchPromptlyDyn <$> widgetHold (listLeftMostClick $ listWithElems []) (constructCollectionList <$> colls)

          -- config widget
          ankiConfig <- ankiConfigWidget ankiLoaded

          -- start anki button
          startButtonPressed <- buttonClass "start-anki-button" "Start"

          return (startButtonPressed, ankiNameSelected, ankiConfig)

    ankiLoaded <- getAnki ankiNameSelected
    loadedAnkiDyn <- holdDyn Nothing ankiLoaded

    let startAnkiE = attachDynWith const loadedAnkiDyn startButtonPressed

    return ()
  where

    ankiLoadedWithConfig :: Reflex t => Dynamic t AnkiConfig -> Event t (Maybe Anki) -> Event t (Maybe (Anki, AnkiConfig))
    ankiLoadedWithConfig = attachDynWith maybeHelper
      where
        maybeHelper _ Nothing = Nothing
        maybeHelper d (Just e) = Just (e, d)

    getTickCounter :: MonadWidget t m => Int -> m (Event t ())
    getTickCounter interval = void <$> (liftIO getCurrentTime >>= tickLossy (realToFrac interval))

    constructCollectionList :: MonadWidget t m => Maybe [String] -> m (Event t String)
    constructCollectionList Nothing = listLeftMostClick $ listWithElems []
    constructCollectionList (Just xs ) = listLeftMostClick . listWithElems . map collectionToListElem $ xs



------------------------ ANKI WIDGET -----------------------------------------

maybeAttach :: a -> Maybe b -> Maybe (a,b)
maybeAttach _ Nothing  = Nothing
maybeAttach a (Just b) = Just (a,b)

-- workflow :: forall t m a. MonadWidget t m => Workflow t m a -> m (Dynamic t a)
ankiWorkflowWidget :: MonadWidget t m =>
                      Event t (Maybe (Anki, AnkiConfig)) -> m (Event t (Maybe (AnkiStep, Answer)))
ankiWorkflowWidget ema = do
  let mAp            = (fmap.fmap) (uncurry startAnki) ema
      workflowWidget = widgetHold (questionWorkflow Nothing) (fmap questionWorkflow mAp)

  (switchPromptlyDyn . joinDyn) <$> workflowWidget
      where
        questionWorkflow = workflow . buildAnkiDisplayWorkflow


buildAnkiDisplayWorkflow :: MonadWidget t m => Maybe AnkiProgress -> Workflow t m (Event t (Maybe (AnkiStep, Answer)))
buildAnkiDisplayWorkflow Nothing   = Workflow $ nothingWidget *> return (never, never)
  where
    nothingWidget = divClass' "anki-widget-main-text" $ text "Anki not loaded."

buildAnkiDisplayWorkflow (Just ap) = ankiSeriesProcessingWorkflow final ankiAnswerWidget ap
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

      let wantsAnswer = const Answering <$> leftmost [domEvent Click e]
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
          g <- buttonClass "button right" "Good"
          b <- buttonClass "button" "Bad"
          return (g,b)
        return $ leftmost [const Good <$> goodE, const Bad <$> badE]


ankiWidget :: MonadWidget t m => Question -> m (Event t Answer)
ankiWidget = (snd <$>) . ankiWidget'


-------------------------------- RIGHT PANE LAYOUT --------------------------------------

data ListElem a = ListElem { _listElemValue   :: a
                           , _listElemImgPath :: Maybe FilePath
                           , _listElemHeader  :: Text
                           , _listContentText :: Text} deriving (Show, Eq, Functor)

newtype AnswerEvent = AnswerEvent Answer
  deriving (Read,Show,Eq,Ord)


data AnswerState = Prompting | Answering
                   deriving (Eq,Show,Ord,Enum)


collectionToListElem :: String -> ListElem String
collectionToListElem s = ListElem s (Just "./resources/kosmici.jpg")
                                  (T.concat ["Collection: ", T.pack s])
                                  (T.concat ["Description of ", T.pack s, ": Lorem ipsum dolor sit amet"])

listLeftMostClick :: MonadWidget t m => m [(El t, a)] -> m (Event t a)
listLeftMostClick list' = leftmost <$> fmap (map clickHelp) list'
  where
    clickHelp :: Reflex t => (El t, a) -> Event t a
    clickHelp (e, a) = const a <$> domEvent Click e

listWithElems :: MonadWidget t m => [ListElem a] -> m [(El t, a)]
listWithElems = listGroup . mapM createLiWidget
  where
    listGroup = elClass "ul" "list-group"
    createLiWidget (ListElem v mImgP header txt) =
      elClass' "li" "list-group-item hvr-backward" $ do
      _ <- myFmapMaybe createImgPath mImgP
      _ <- divClass "media-body" $ do
        el "strong" $ text (T.unpack header)
        el "p" $ text (T.unpack txt)
      return v

createImgPath :: MonadWidget t m => FilePath -> m ()
createImgPath imgP = let (w,h) = listElemImgSize
                      in elAttr "img"
                        (fromList [ ("class", "img-circle media-object pull-left")
                                  , ("src", imgP)
                                  , ("width", show w)
                                  , ("height", show h)])
                        (return ())

listElemImgSize :: (Int,Int)
listElemImgSize = (32,32)


completionInfoWidget :: MonadWidget t m => Event t (Maybe Anki) -> Event t (Maybe (AnkiStep,Answer)) -> m (Dynamic t (Int, Int))
completionInfoWidget ankiLoaded maaE = divClass "completion-info-widget" $ do
  -- ankistate, reseted by ankiLoaded
  ankiDyn <- holdDyn Nothing ankiLoaded
  ankiStateDyn <- holdDyn Nothing (leftmost [const Nothing <$> ankiLoaded,maaE])
  totalandLeftDyn <- combineDyn completionInfo ankiDyn ankiStateDyn
  completionDynStr <- mapDyn (\(total,left) -> show (debugPrint total - debugPrint left :: Int) ++ "/" ++ show total) totalandLeftDyn
  text "Stan ukończenia: "
  dynText completionDynStr
  return totalandLeftDyn

  where
    completionInfo Nothing               _       = (0,0)
    completionInfo (Just (Anki _ allQs)) Nothing = let l = length allQs
                                                   in (l,l)
    completionInfo _ (Just (ASDone x,_))     = (length x, 0)
    completionInfo _ (Just (AStep (q:|qs) x,_)) = let left = length (q:qs)
                                                      all'  = left + length x
                                                  in (all', left)


accuracyInfoWidget :: MonadWidget t m => Event t (Maybe Anki) -> Event t (Maybe (AnkiStep,Answer)) -> m (Dynamic t (Int, Int))
accuracyInfoWidget ankiLoaded maaE = divClass "accuracy-info-widget" $ do
  goodRatio <- foldDyn addAnswer (0,0) (leftmost [const Nothing <$> ankiLoaded,Just <$> maaE])
  accuracyDynStr <- mapDyn (\(good,total) -> show good ++ "/" ++ show total) goodRatio
  text "Wskaźnik poprawności: "
  dynText accuracyDynStr
  return goodRatio

  where
    addAnswer Nothing _ = (0,0)
    addAnswer (Just Nothing) x = x
    addAnswer (Just (Just (_,Good))) (g,a) = (g+1,a+1)
    addAnswer (Just (Just (_,Bad))) (g,a) = (g,a+1)


--------------------- Config widget ----------------------------

ankiConfigWidget :: MonadWidget t m => Event t (Maybe Anki) -> m (Dynamic t AnkiConfig)
ankiConfigWidget maE = do

  comboLen <- comboInputWidget
  queSel <- questionSelectorWidget maE

  divClass "anki-config-widget" $ combineDyn MkAnkiConfig comboLen queSel
  where
    comboInputWidget = text "Combo" *> (numberInput >>= filterDynamicChanges readMaybe 3)

filterDynamicChanges :: MonadWidget t m => (a-> Maybe b) -> b -> Dynamic t a -> m (Dynamic t b)
filterDynamicChanges p initial dynamic = holdDyn initial (fmapMaybe p $ updated dynamic)

numberInput :: MonadWidget t m => m (Dynamic t String)
numberInput = _textInput_value <$> divClass "numberInput" numInp
  where
    numInp = textInput $ def & textInputConfig_inputType .~ "number"
      & textInputConfig_initialValue .~ "0"


------- question selection

widgetSelector :: MonadWidget t m => Event t (m a)
widgetSelector = undefined


questionSelectorWidget :: MonadWidget t m => Event t (Maybe Anki) -> m (Dynamic t QuestionSelector)
questionSelectorWidget dynMA = joinDyn <$> widgetHold startWidget (buildWidgetEvent dynMA)
  where
    startWidget :: MonadWidget t m => m (Dynamic t QuestionSelector)
    startWidget = divClass "question-selector" $ return (constDyn QsAll)

    checkListWidget :: MonadWidget t m => [Question] -> m (Dynamic t QuestionSelector)
    checkListWidget qs = divClass "question-selector" (checkList questionSelectCheckbox qs) >>= mapDyn QsSelected

    buildWidgetEvent :: MonadWidget t m => Event t (Maybe Anki) -> Event t (m (Dynamic t QuestionSelector))
    buildWidgetEvent = fmap (\case
                                Nothing            -> startWidget
                                (Just (Anki _ qs)) -> checkListWidget qs)

    questionSelectCheckbox (Question _ _ qText) = divClass "question-checkbox" ((_checkbox_value <$> checkbox True def) <* text (T.unpack qText))


checkList :: MonadWidget t m => (a -> m (Dynamic t Bool)) -> [a] -> m (Dynamic t [a])
checkList checkWidget items = do
  -- dynamic map of bindings (a -> Bool)
  dynMap <- joinDynThroughMap . constDyn . fromList . zip indices <$> mapM checkWidget items
  -- filter only checked values indices
  checkdIdx <- mapDyn (M.keys . M.filter id) dynMap
  -- select checked items
  mapDyn (\idxx -> fastFilterIdx [] idxx (zip indices items)) checkdIdx
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

          --     widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
widgetHoldHelper :: MonadWidget t m
    => (a -> m b) -> a -> Event t a
    -> m (Dynamic t b)
widgetHoldHelper f eDef e = widgetHold (f eDef) (f <$> e)


------------------- OTHER AUXILIARY -----------------------------

myFmapMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
myFmapMaybe f (Just v) = sequence . Just . f $ v
myFmapMaybe _ Nothing  = return Nothing

