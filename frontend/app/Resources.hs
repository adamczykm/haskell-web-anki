{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RankNTypes                 #-}

module Resources where

import           Control.Monad      (join)
import           Control.Monad.Free
import           Data.Bool          (bool)
import           Data.Either        (partitionEithers)
import           Data.Function      (on)
import           Data.List          (groupBy, sort)
import           Data.Text          (pack)
import           Prelude            hiding (readFile)
import           System.Directory
import           System.FilePath
import           System.IO.Error

import           Domain

-------------------------------------------------------------------------------
----- monadic interface for loading app resources into the scope of computation

reifyResource :: (forall m. MonadResource m => m a) -> Free ResourceF a
reifyResource = unFreeResource

newtype FreeResource a = FreeResource { unFreeResource :: Free ResourceF a } deriving (Functor, Applicative, Monad)

data ResourceError = FileSystemError String
                   | InvalidResourceStructure String
                   | OtherIOError IOError
                   deriving (Show, Eq)

class Monad m => MonadResource m where
    loadQuestions :: FilePath -> m ([ResourceError], [Question])

data ResourceF next
    = LoadQuestions FilePath (([ResourceError], [Question]) -> next)

instance Functor ResourceF where
  fmap f (LoadQuestions p n) = LoadQuestions p (f . n)

instance MonadResource FreeResource where
    loadQuestions path = FreeResource (liftF (LoadQuestions path id))

instance MonadResource IO where
  loadQuestions = undefined



-------------------------------------------------------------------------------
-------- auxilliary

-- loadQuestionsIO :: FilePath -> IO ([ResourceError], [Question])
-- loadQuestionsIO = fmap partitionEithers . join . fmap (either resourceDirError readQuestions) . readResourceDir
--   where

--     resourceDirError :: ResourceError -> IO [Either ResourceError Question]
--     resourceDirError err = return [Left err]

--     -- readQuestion :: FilePath -> IO (Either ResourceError Question)
--     -- readQuestion fp = do
--     --   let imgp = fp -<.> "png"
--     --   doesFileExist imgp >>= bool
--     --      (return $ Left $ FileSystemError ("Image path: " ++ show imgp ++ " doesn't exist."))
--     --      (return $ Right (Question (takeBaseName fp) imgp (pack $ takeBaseName fp)))

--     readQuestion :: FilePath -> IO (Either ResourceError Question)
--     readQuestion fp = do
--       let ansp = fp -<.> "txt"
--       let imgp = fp -<.> "png"
--       doesFileExist ansp >>= bool
--         (return $ Left $ FileSystemError ("Answer path: " ++ show ansp ++ " doesn't exist."))
--         (doesFileExist imgp >>= bool
--             (return $ Left $ FileSystemError ("Image path: " ++ show imgp ++ " doesn't exist."))
--             (do
--               ans <- readFile ansp
--               return $ Right (Question (takeBaseName fp) imgp ans)))

--     readResourceDir :: FilePath -> IO (Either ResourceError [FilePath])
--     readResourceDir p = doesDirectoryExist p >>= \case
--        True -> fmap (either (Left . OtherIOError) Right)
--                     (tryIOError $ map (p </>) <$> listDirectory p)
--        False -> return $ Left $ FileSystemError ("Path " ++ show p ++ " doesn't exist.")

--     readQuestions :: [FilePath] -> IO [Either ResourceError Question]
--     readQuestions = mapM (readQuestion . head) . groupBy ((==) `on` takeBaseName) . sort
