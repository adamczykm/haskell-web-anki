{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module ApiClient where

import           Reflex
import           Reflex.Dom
import           System.FilePath
import Control.Monad.IO.Class (liftIO)
import System.Random.Shuffle (shuffleM)

import Domain

serverUrl :: String
serverUrl = "http://localhost:8666"
-- serverUrl = "http://92.222.85.52:8666"

getCollectionsInfo :: (MonadWidget t m) => Event t a -> m (Event t (Maybe [CollectionInfo]))
getCollectionsInfo = getAndDecode . fmap (const $ joinPath [serverUrl, "collections"])


getAnki :: (MonadWidget t m) => Event t CollectionInfo -> m (Event t Anki)
getAnki ciE = do
  let nameE = _ciName <$> ciE
  -- get question from server and shuffle them if correctly downloaded
  qsE <- fmapMaybe (>>= Just) <$> getAndDecode (ankiUrl <$> nameE)
  -- -- sort questions
  -- let shuffledQsE = fmap (liftIO . shuffleM) (qsE :: Event t [Question])
  nameDyn <- hold "" nameE

  return $ uncurry Anki <$> attach nameDyn qsE

  where
    ankiUrl fn = joinPath [serverUrl, "questions", fn]

imgSrcForQuestion :: Question -> String
imgSrcForQuestion (Question _ p _) = joinPath [serverUrl, p]
