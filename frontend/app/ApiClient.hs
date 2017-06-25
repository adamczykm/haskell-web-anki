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

-- instance Mona

getAnki :: (MonadWidget t m) => Event t CollectionInfo -> m (Event t (Maybe Anki))
getAnki ciE = do
  mQsE   <- getAndDecode (ankiUrl . _ciName <$> ciE)
  mNameE <- hold (MkCollectionInfo "" 0) ciE

  let anki = flip fmap (attach mNameE mQsE) $ \case
                (_, Nothing) -> Nothing
                (n', Just qs) -> Just $ Anki (_ciName n') qs

  let tmp = (fmap . fmap) (\(Anki n' qs) -> do
                              sqs <- liftIO (shuffleM qs)
                              return (Anki n' sqs))
            anki

  performEvent $ fmap sequence tmp

  where
    ankiUrl fn = joinPath [serverUrl, "questions", fn]

imgSrcForQuestion :: Question -> String
imgSrcForQuestion (Question _ p _) = joinPath [serverUrl, p]
