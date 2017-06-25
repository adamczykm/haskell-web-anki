{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Domain where

import           Data.Aeson.TH
import           Data.List     (elemIndex)
import           Data.Maybe    (fromMaybe)
import           Data.Text     (Text)
import Data.Bool (bool)
import NonEmpty

import Debug

-- import           Data.Ord        (comparing)
-- type Question = (FilePath, Text)

data Question = Question { qsId :: String, imgPath :: FilePath, question :: Text}
              deriving (Eq, Show)

$(deriveJSON defaultOptions ''Question)

data Answer = Good | Bad
            deriving (Show, Eq, Read, Ord)

data CollectionInfo = MkCollectionInfo {_ciName :: String, _ciQuestionCount :: Int}
  deriving(Eq,Show)

$(deriveJSON defaultOptions ''CollectionInfo)

-- data QuestionMemoState = QuestionMemoState {

-- data CollectionMemoState = CollectionMemoState {
--   creationDate :: UTCTime,
--   collection :: String,
--   questionState ::
--                              }


---------- simple safe combo interface

comboLength :: Integer
comboLength = 3

-- hide this constructors
data Combo = InProgress Integer
           | Completed
           deriving(Show, Eq)

$(deriveJSON defaultOptions ''Combo)

-- newCombo :: Combo
-- newCombo = InProgress 0

-- resetCombo :: Combo -> Combo
-- resetCombo = const $ InProgress 0

-- progressCombo :: Combo -> Combo
-- progressCombo Completed      = Completed
-- progressCombo (InProgress x)
--   | x + 1 >= comboLength     = Completed
--   | otherwise                = InProgress (x+1)

-- isCompleted :: Combo -> Bool
-- isCompleted Completed = True
-- isCompleted _         = False

----------- alternative question state

data QuestionState = QuestionState {
  qsQuestion   :: Question,
  qsAnswers    :: [Answer]}
  deriving (Eq, Show)


qsGoodAnwers = length . filter (== Good) . qsAnswers
qsAllAnwers = length . qsAnswers


newQuestionState qs = QuestionState qs []


checkCombo comboLen (QuestionState _ xs) = if cLength < comboLen
                                     then InProgress cLength
                                     else Completed
  where
    cLength = fromIntegral $ fromMaybe (length xs) (elemIndex Bad xs)


isCompleted :: Integer -> QuestionState -> Bool
isCompleted cLength = (== Completed) . checkCombo cLength

----------- question state

-- data QuestionState = QuestionState {
--   qsQuestion   :: Question,
--   qsCombo      :: Combo,
--   qsGoodAnwers :: Int,
--   qsAnswers    :: Int}
--   deriving (Eq, Show)



-- newQuestionState :: Question -> QuestionState
-- newQuestionState qs = QuestionState qs (InProgress 0) 0 0

----------- anki series

data Anki = Anki String [Question]
  deriving (Eq,Show)

data QuestionSelector = QsAll | QsSelected [Question]
  deriving (Eq,Show)


data AnkiConfig = MkAnkiConfig{ _comboLength :: Integer
                              , _questionFilter :: QuestionSelector}
  deriving (Eq, Show)

data AnkiProgress = MkAnkiProgress Anki AnkiStep AnkiConfig
  deriving (Eq, Show)

data AnkiStep = AStep (NonEmpty QuestionState) [QuestionState]
              | ASDone [QuestionState]
  deriving (Eq,Show)

answeredClosestLocation :: [a] -> Int
answeredClosestLocation xs = intRound (1.0/3.0 * fromIntegral (length xs))
  where
    intRound :: Double -> Int
    intRound = round


defaultAnkiConfig :: AnkiConfig
defaultAnkiConfig = MkAnkiConfig 3 QsAll

startAnki :: Anki -> AnkiConfig -> AnkiProgress
startAnki a@(Anki _ [])        cfg =
  MkAnkiProgress a (ASDone []) cfg
startAnki a                    cfg@(MkAnkiConfig _ (QsSelected [])) =
  MkAnkiProgress a (ASDone []) cfg
startAnki a                    cfg@(MkAnkiConfig _ (QsSelected (x:xs))) =
  MkAnkiProgress a (AStep (newQuestionState x :| map newQuestionState xs) []) cfg
startAnki a@(Anki _ (x:xs))    cfg@(MkAnkiConfig _ QsAll) =
  MkAnkiProgress a (AStep (newQuestionState x :| map newQuestionState xs) []) cfg


ankiStepWithAnswer :: AnkiConfig -> AnkiStep -> (Question -> Answer) -> (AnkiStep, Maybe Answer)
ankiStepWithAnswer cfg as f =
  let (_, answer, newAs) = ankiApplyAnswer as
      in (newAsOrder newAs, answer)

  where
    comboLen = _comboLength cfg

    ankiApplyAnswer (ASDone x) = (False, Nothing, ASDone x)
    ankiApplyAnswer (AStep (qs' :| qss') asnwrd)
      = let (ans, newQs) = answerQuestion qs'
            (completed, newAs) = ankiUpdateState (AStep (newQs :| qss') asnwrd)
        in (completed, Just ans, newAs)
      where
        answerQuestion (QuestionState q anss) = let ans = f q in
          (ans, QuestionState q (ans : anss))

        ankiUpdateState    (ASDone x)            = (False, ASDone x)
        ankiUpdateState as'@(AStep (qs:|qss) answrd)
          | isCompleted comboLen qs = (True, case qss of
              []      -> ASDone (qs:answrd)
              (x:xs)  -> AStep  (x:|xs) (qs:answrd))
          | otherwise      = (False, as')

    newAsOrder (ASDone x) = ASDone x
    newAsOrder (AStep (qs:|qss) answrd) =
      let (bs,cs)  = splitAt (answeredClosestLocation qss) qss
          (nq:nqs) = bs ++ insertFunction qs cs
      in AStep (nq:|nqs) answrd
      where
        insertFunction answd []          = [answd]
        insertFunction answd rest@(x:xs) = if eFactor answd < eFactor x
                                           then answd : rest
                                           else x : insertFunction answd xs

        eFactor :: QuestionState -> Double
        eFactor qs' = fromIntegral (qsGoodAnwers qs) / fromIntegral (qsAllAnwers qs' + 1)

    -- sortByDifficulty = sortBy (comparing (\(QuestionState _ _ ans' asked') -> (fromIntegral ans' / fromIntegral asked') :: Double))


-- ankiStepWithAnswer :: AnkiStep -> (Question -> Answer) -> AnkiStep
-- ankiStepWithAnswer ASDone _ = ASDone
-- ankiStepWithAnswer (AStep qs@(QuestionState q c ans asked) qss) f =
--   case f (qsQuestion qs) of
--     Good -> case progressCombo c of
--       c'@(InProgress _) -> prepareStep (QuestionState q c' (ans+1) (asked+1):qss)
--       Completed         -> prepareStep qss
--     Bad  -> prepareStep (QuestionState q (resetCombo c) ans (asked+1):qss)

--   where
--     prepareStep :: [QuestionState] -> AnkiStep
--     prepareStep []  = ASDone
--     prepareStep (q':qs') = let (bs,as) = splitAt (answeredClosestLocation qs') qs'
--                                (nq:nqs) = bs ++ insertFunction q' as
--                            in AStep nq nqs

--     insertFunction answd []          = [answd]
--     insertFunction answd rest@(x:xs) = if eFactor answd > eFactor x
--                                        then answd : rest
--                                        else x : insertFunction answd xs

--     eFactor :: QuestionState -> Double
--     eFactor (QuestionState _ _ ans' asked') = 1.0 - (fromIntegral ans' / (fromIntegral asked'+1))


-- startAnki :: Anki -> AnkiProgress
-- startAnki a@(Anki _ [])     = AnkiProgress a ASDone
-- startAnki a@(Anki _ (x:xs)) = AnkiProgress a (AStep (newQuestionState x)
--                                              (map newQuestionState xs))


-- data AnkiStep = AStep QuestionState [QuestionState]
--               | ASDone
--   deriving (Eq,Show)
