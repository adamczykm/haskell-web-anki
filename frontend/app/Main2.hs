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

module Main2 where

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
import NlpCore

import Data.Function (on)

import Domain
import ReflexDomApp
import Services
import ElectronApi
import ApiClient

main :: IO ()
main = runReflexDomApp testServiceApp productionServices

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

questions :: Int -> [Question]
questions n = map (\i -> Question (show i) "./resources/kosmici.jpg" (T.pack ("Kosmici" ++ show i ++ "?"))) [1..n]

testServiceApp :: ReflexDomApp()
testServiceApp = ReflexDomApp mainLayout3

logShit :: Monad m => ReflexServiceFun 'LoggingService m ()
logShit = callWithLogService $ logM "shit"

logShit2 :: Monad m => ReflexServiceFun 'ElectronApiService m ()
logShit2 = callWithElectronApiService $ consoleLog "shit"
  -- showOpenDialog


-- newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

-- workflow :: forall t m a. MonadWidget t m => Workflow t m a -> m (Dynamic t a)
ankiWorkflowWidget :: MonadWidget t m =>
                      Event t (Maybe Anki) ->
                      m (Event t (Maybe (AnkiStep, Answer)))
-- ankiWorkflowWidget emq = undefined
ankiWorkflowWidget ema = do

  let mAp            = (fmap.fmap) startAnki ema
      workflowWidget = widgetHold (questionWorkflow Nothing) (fmap questionWorkflow mAp)

  (switchPromptlyDyn . joinDyn) <$> workflowWidget
      where
        questionWorkflow = workflow . buildAnkiDisplayWorkflow




buildAnkiDisplayWorkflow :: MonadWidget t m => Maybe AnkiProgress -> Workflow t m (Event t (Maybe (AnkiStep, Answer)))

buildAnkiDisplayWorkflow Nothing   = Workflow $ nothingWidget *> return (never, never)
  where
    nothingWidget = divClass' "anki-wiget-main-text" $ text "Anki not loaded."

buildAnkiDisplayWorkflow (Just ap) = ankiSeriesProcessingWorkflow final ankiAnswerWidget ap
  where
    final _ = do
      (e, _) <- divClass' "anki-wiget-main-text" $ text "Koniec"
      return (domEvent Click e)

    ankiSeriesProcessingWorkflow :: MonadWidget t m =>
        (AnkiProgress -> m (Event t ())) ->
        -- ^ Widget for finalization (all list element processed)
        (AnkiProgress -> m (Event t Answer)) ->
        -- ^ main widget for processing list element
        AnkiProgress ->
        -- ^ list of elements to be processed
        Workflow t m (Event t (Maybe (AnkiStep, Answer)))
        -- ^ resulting worklow
    ankiSeriesProcessingWorkflow finalW w' (AnkiProgress a astep) = go finalWNothing (constant astep) w'
      where

        ankiProgress :: AnkiStep -> AnkiProgress
        ankiProgress = AnkiProgress a

        finalWNothing = Workflow $ do
          e <- finalW $ ankiProgress astep
          return (const Nothing <$> e, never)

        --newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }
        go :: MonadWidget t m =>
              Workflow t m (Event t (Maybe (AnkiStep, Answer))) ->
              Behavior t AnkiStep ->
              (AnkiProgress -> m (Event t Answer)) ->
              Workflow t m (Event t (Maybe (AnkiStep, Answer)))

        go wf asB w = Workflow $ do
          as' <- sample asB
          case as' of
            ASDone -> unWorkflow wf
            _      -> do
              ans <- w (ankiProgress as')
              let aE = fmap (as',) ans
              newAsB <- switcher asB (constant . (\(as, ans') -> ankiStepWithAnswer as (const ans')) <$> aE)
              return (Just <$> attach newAsB ans, const (go wf newAsB w) <$> aE)

questionWorkflowWidget :: MonadWidget t m => Event t (Maybe [Question]) -> m (Event t (Maybe (Question, Answer)))
questionWorkflowWidget emq = do
  let eq = discardNothingEvent emq
  temp <- holdDyn "0" $ show <$> eq
  dynText temp
  (switchPromptlyDyn . joinDyn) <$> widgetHold (questionWorkflow []) (fmap questionWorkflow eq)
    where
      questionWorkflow = workflow . buildDisplayWorkflow

      discardNothingEvent = fmap (\case
                                    Nothing -> []
                                    Just xs -> xs)

mainLayout3 :: MonadWidget t m => ReflexServiceFun3 'ElectronApiService 'ResourceService 'LoggingService m ()
mainLayout3 =
  nestDivClasses ["window", "window-content", "pane-group"] $ mdo
    _ <- divClass "pane" $ do
        -- ansE <- questionWidget sampleQuestion
        ansE <- ankiWorkflowWidget ankiLoaded
        return ()

        -- goods <- foldDyn incGood 0 ansE
        -- goodStr <- holdDyn "1" $ show <$> updated goods
        -- dynText goodStr

        -- bads <- foldDyn incBad 0 ansE
        -- badStr <- holdDyn "0" $ show <$> updated bads
        -- dynText badStr

    ankiNameSelected <- divClass "pane-sm sidebar padded" $ do
          -- get available collections from server every n sec.
          colls <- liftM2 (<>) getPostBuild (getTickCounter 10) >>= getAnkiNamesList

          -- construct dynamic list with available collections
          -- and stores event of clicking one of its elements
          -- -- widgetHold :: MonadWidget t m => m a -> Event t (m a) -> m (Dynamic t a)
          switchPromptlyDyn <$> widgetHold (listLeftMostClick $ listWithElems []) (constructCollectionList <$> colls)

    ankiLoaded <- debugPrintF <$> getAnki ankiNameSelected

    return ()
  where
    getTickCounter :: MonadWidget t m => Int -> m (Event t ())
    getTickCounter interval = void <$> (liftIO getCurrentTime >>= tickLossy (realToFrac interval))

    constructCollectionList :: MonadWidget t m => Maybe [String] -> m (Event t String)
    constructCollectionList Nothing = listLeftMostClick $ listWithElems []
    constructCollectionList (Just xs ) = listLeftMostClick . listWithElems . map collectionToListElem $ xs

    incGood :: Maybe (AnkiStep, Answer) -> Int -> Int
    incGood (Just (_, Good)) n = n+1
    incGood _ n = n

    incBad :: Maybe (AnkiStep, Answer) -> Int -> Int
    incBad (Just (_, Bad)) n = n+1
    incBad _ n = n


mainLayout :: MonadWidget t m => m ()
mainLayout =
  nestDivClasses ["window", "window-content", "pane-group"] $ mdo
    rewrites <- divClass "pane-sm sidebar padded" $ do
      r <- wordsAccumWidget wordsCountMap
      _ <- uniqueWordsCountsList wordsCountMap
      return r
    wordsCountMap <- divClass "pane padded" $ mainTextArea rewrites

    _ <- divClass "pane-sm sidebar padded" $
         listWithElems (exampleList  20)
    return ()

data ListElem a = ListElem { _listElemValue   :: a
                           , _listElemImgPath :: Maybe FilePath
                           , _listElemHeader  :: Text
                           , _listContentText :: Text} deriving (Show, Eq, Functor)

newtype AnswerEvent = AnswerEvent Answer
  deriving (Read,Show,Eq,Ord)


data AnswerState = Prompting | Answering
                   deriving (Eq,Show,Ord,Enum)

widgetHoldHelper :: MonadWidget t m
    => (a -> m b) -> a -> Event t a
    -> m (Dynamic t b)
widgetHoldHelper f eDef e = widgetHold (f eDef) (f <$> e)


listProcessingWorkflow :: MonadWidget t m =>
                          m (Event t ()) ->
                          -- ^ Widget for finalization (all list element processed)
                          (a -> m (Event t b)) ->
                          -- ^ main widget for processing list element
                          [a] ->
                          -- ^ list of elements to be processed
                          Workflow t m (Event t (Maybe (a,b)))
                          -- ^ resulting worklow
listProcessingWorkflow finalW w xs' = go xs' finalWNothing
  where
    finalWNothing = Workflow $ do
      e <- finalW
      return (const Nothing <$> e, never)
    go [] wf = wf
    go (x:xs) wf = Workflow $ do
                aE <- w x
                return (Just . (\y -> (x,y)) <$> aE, const (go xs wf) <$> aE)


listProcessingWorkflow2 :: MonadWidget t m =>
                          m (Event t ()) ->
                          -- ^ Widget for finalization (all list element processed)
                          (a -> m (Event t b)) ->
                          -- ^ main widget for processing list element
                          Event t (Maybe [a]) ->
                          -- ^ list of elements to be processed
                          Workflow t m (Event t (Maybe (a,b)))
                          -- ^ resulting worklow
listProcessingWorkflow2 = undefined
-- listProcessingWorkflow2 finalW w xs' = go xs' finalWNothing
-- listProcessingWorkflow2 finalW w xs' = go xs' finalWNothing
--   where
--     finalWNothing = Workflow $ do
  --       e <- finalW
--       return (const Nothing <$> e, never)
--     go [] wf = wf
--     go (x:xs) wf = Workflow $ do
--                 aE <- w x
--                 return (Just . (\y -> (x,y)) <$> aE, const (go xs wf) <$> aE)

buildDisplayWorkflow2 :: MonadWidget t m => [Question] -> Workflow t m (Event t (Maybe (Question, Answer)))
buildDisplayWorkflow2 = listProcessingWorkflow final questionWidget
  where
    final = do
      (e, _) <- elAttr' "div" M.empty $ text "Koniec"
      return (domEvent Click e)


          -- sample ankiProgress >>= \case
          --   (AnkiProgress _ ASDone)          -> wf
          --   ap@(AnkiProgress _ as@(AStep x xs)) -> Workflow $ do
          --   aE <- w ap
          --   return (Just . (\y -> (ankiStepWithAnswer as (const y), y)) <$> aE, const (go wf (constant ap)) <$> aE)


-- buildAnkiDisplayWorkflow :: MonadWidget t m => AnkiProgress -> m (Workflow t m (Event t (Maybe (AnkiStep, Answer))))
-- buildAnkiDisplayWorkflow = ankiSeriesProcessingWorkflow final undefined --questionWidget
--   where

--     final _ = do
--       (e, _) <- elAttr' "div" M.empty $ text "Koniec"
--       return (domEvent Click e)


    -- ankiSeriesProcessingWorkflow :: MonadWidget t m =>
    --                           (AnkiProgress -> m (Event t ())) ->
    --                           -- ^ Widget for finalization (all list element processed)
    --                           (AnkiProgress -> m (Event t Answer)) ->
    --                           -- ^ main widget for processing list element
    --                           AnkiProgress ->
    --                           -- ^ list of elements to be processed
    --                           (Workflow t m (Event t (Maybe (AnkiStep, Answer))))
    --                           -- ^ resulting worklow
    -- ankiSeriesProcessingWorkflow finalW w (AnkiProgress a as) = go w finalWNothing
    --   where

    --     ankiProgress :: AnkiStep -> AnkiProgress
    --     ankiProgress = AnkiProgress a

    --     finalWNothing = return $ Workflow $ do
    --       e <- finalW (ankiProgress as)
    --       return (const Nothing <$> e, never)

    --     go :: MonadWidget t m =>
    --           (AnkiProgress -> m (Event t Answer)) ->
    --           m (Workflow t m (Event t (Maybe (AnkiStep, Answer)))) ->
    --           Behavior t AnkiProgress ->
    --           (Workflow t m (Event t (Maybe (AnkiStep, Answer))))
    --     go w wf ankiProgress = sample ankiProgress >>= \case
    --         (AnkiProgress _ ASDone)          -> wf
    --         ap@(AnkiProgress _ as@(AStep x xs)) -> do
    --           nextWorkflowStep <- go w wf (constant ap)
    --           return $ Workflow $ do
    --             aE <- w ap
    --             return (Just . (\y -> (ankiStepWithAnswer as (const y), y)) <$> aE, const nextWorkflowStep <$> aE)

--newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a)) }

buildDisplayWorkflow :: MonadWidget t m => [Question] -> Workflow t m (Event t (Maybe (Question, Answer)))
buildDisplayWorkflow = listProcessingWorkflow final questionWidget
  where
    final = do
      (e, _) <- elAttr' "div" M.empty $ text "Koniec"
      return (domEvent Click e)


-- questionWidget'' :: MonadWidget t m => Question -> m (El t, Event t Answer)
-- questionWidget'' q = do
--   rec (e, viewer) <- divClass' "question-viewer" $ do
--         de <- widgetHoldHelper (state2Widget q) Prompting wantsAnswer
--         return $ switch $ current de
--       let wantsAnswer = const Answering <$> domEvent Click e
--       -- ^ Event after which the widget will get into the Answering state.
--   return (e, viewer)
--     where
--       state2Widget (Question _ _ t) Prompting = promptViewer t
--       state2Widget q' Answering = answViewer q'

--       promptViewer questionTxt = divClass "question-prompt-viever" $ do
--         text $ T.unpack questionTxt
--         return never

--       answViewer :: MonadWidget t m => Question -> m (Event t Answer)
--       answViewer q' = do
--         (_, _) <- divClass' "answer-viewer" $ fullSizeCenteredImg (imgSrcForQuestion q')
--         goodE <- button "Good"
--         badE <- button "Bad"
--         return $ leftmost [const Good <$> goodE, const Bad <$> badE]

questionWidget' :: MonadWidget t m => Question -> m (El t, Event t Answer)
questionWidget' q = do
  rec (e, viewer) <- divClass' "anki-widget" $ do
        de <- widgetHoldHelper (state2Widget q) Prompting wantsAnswer
        return (switch $ current de)

      let wantsAnswer = const Answering <$> leftmost [domEvent Click e]
      -- ^ Event after which the widget will get into the Answering state.
 
  return (e, viewer)
    where

      state2Widget q' Prompting = promptViewer q'
      state2Widget q' Answering = answViewer q'

      promptViewer :: MonadWidget t m => Question -> m (Event t Answer)
      promptViewer q'' = divClass "question-container" $ do
        _ <- spanClass' "question-text" $ text  (T.unpack $ question q'')
        return never

      answViewer :: MonadWidget t m => Question -> m (Event t Answer)
      answViewer q' = do
        _ <- divClass "ans-img-container" $ imgClass' "ans-img" (imgSrcForQuestion q')
        (goodE, badE) <- divClass "ans-buttons-container" $ do
          g <- buttonClass "button right" "Good"
          b <- buttonClass "button" "Bad"
          return (g,b)
        return $ leftmost [const Good <$> goodE, const Bad <$> badE]

-- first it shows some question, then if u click on it it will show img with the answer, and after that you choose whether 

ankiAnswerWidget :: MonadWidget t m => AnkiProgress -> m (Event t Answer)
ankiAnswerWidget (AnkiProgress _ (AStep q _)) = (snd <$>) . questionWidget' $ qsQuestion q

questionWidget :: MonadWidget t m => Question -> m (Event t Answer)
questionWidget = (snd <$>) . questionWidget'


collectionToListElem :: String -> ListElem String
collectionToListElem s = ListElem s (Just "./resources/kosmici.jpg")
                                  (T.concat ["Collection: ", T.pack s])
                                  (T.concat ["Description of ", T.pack s, ": Lorem ipsum dolor sit amet"])

exampleList :: Int -> [ListElem Int]
exampleList n = map exampleItem [1..n]
  where
    exampleItem i = ListElem i (Just "./resources/kosmici.jpg")
                             (T.concat ["List Item #", T.pack $ show i])
                             (T.concat ["List Item Content longer text  #", T.pack $ show i])


myFmapMaybe :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
myFmapMaybe f (Just v) = sequence . Just . f $ v
myFmapMaybe _ Nothing  = return Nothing


elClass' :: MonadWidget t m => String -> String -> m a -> m (El t, a)
elClass' elementTag c = elWith' elementTag $ def & attributes .~ "class" =: c

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

imgAttr :: MonadWidget t m => FilePath -> [(String,String)] -> m ()
imgAttr path attrs = elAttr "img" (fromList $ ("src",path) : attrs) $ return ()

tmpImg :: MonadWidget t m => FilePath -> m ()
tmpImg path = el "p" $ elAttr "img" attrs $ return ()
  where attrs = fromList [("src",path),("height","30%"),("width","30%")]

mainTextArea :: MonadWidget t m => Dynamic t [Rewrite] -> m (Dynamic t (TextMap (Sum Integer)))
mainTextArea rewrites = do
  el "p" $ text "Tekst wejściowy"
  inputTextArea rewrites


divId :: MonadWidget t m => String -> m a -> m a
divId i = elAttr "div" ("id" =: i)

uniqueWordsCountsList :: (MonadWidget t m) => Dynamic t (TextMap (Sum Integer)) -> m (Dynamic t [String])
uniqueWordsCountsList wordsCountMap = divId "uniqueWordsCountsList" $ do
  wordsCountLines <- mapDyn (map T.unpack . showTextSortMostFrequent showLine) wordsCountMap
  _ <- simpleList wordsCountLines (el "div" . dynText)
  return wordsCountLines
  where showLine (w,c) = T.concat [w, ": ", (T.pack . show . getSum) c]

wordsAccumWidget :: (MonadWidget t m) => Dynamic t (TextMap (Sum Integer)) -> m (Dynamic t [Rewrite])
wordsAccumWidget wordsCountMap = el "div" $ do
  el "p" $ text "Reguły nadpisywania"
  rewrites <- rewritesTextArea
  text "----------------------------------"
  el "p" $ do
    diff <- mapDyn countDifferentWords wordsCountMap
    total <- mapDyn countAllWords wordsCountMap

    el "p" $ do
      text "Różnych: "
      dynText =<< mapDyn show diff
    el "p" $ do
      text "Wszystkich: "
      dynText =<< mapDyn show total
    el "p" $ do
      text "Stosunek: "
      dynText =<< combineDyn (\d t -> show $ showRatio d t)  diff total

  text "----------------------------------"
  return rewrites
    where
      showRatio :: (Integral a) => a -> Integer -> Float
      showRatio d t = fromIntegral d / fromInteger t

countAllWords :: TextMap (Sum Integer) -> Integer
countAllWords = getSum . mconcat . M.elems

countDifferentWords :: TextMap (Sum Integer) -> Int
countDifferentWords = length . M.keys

rewritesTextArea :: (MonadWidget t m) => m (Dynamic t [Rewrite])
rewritesTextArea = el "div" $ do
 txt' <- _textArea_value <$> textArea (def & (textAreaConfig_initialValue .~ rewritesTextAreaInitial))
 mapDyn (parseRewrites . T.pack) txt'

inputTextArea :: (MonadWidget t m) => Dynamic t [Rewrite] -> m (Dynamic t (TextMap (Sum Integer)))
inputTextArea rewrites = divClass "form-control" $ do
 txt <- _textArea_value <$> textArea (def & (textAreaConfig_initialValue .~ inputTextAreaInitialValue))
 combineDyn countTheWordsFun txt rewrites
   where
     countTheWordsFun txt rws = defaultWordsCount False rws M.empty (T.pack txt)


rewritesTextAreaInitial :: String
rewritesTextAreaInitial = "teskt:=tekst"

inputTextAreaInitialValue :: String
inputTextAreaInitialValue = "Wklej tu teskt, aby zobaczyć liczby wyrazów."




-- leftmostClick :: Reflex t => [El t] -> Event t ()
-- leftmostClick = leftmost . map (domEvent Click)
