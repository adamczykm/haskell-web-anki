{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

module Lib
    ( startApp
    ) where

import           Control.Monad            (filterM, join,foldM)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bool                (bool)
import           Data.Either              (either, partitionEithers)
import           Data.Function            (on)
import           Data.List                (elemIndex, groupBy, isPrefixOf, sort)
import           Data.Text                (Text)
import           Data.Text.IO             (readFile)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude                  hiding (readFile)
import           Servant
import           System.Directory
import           System.FilePath
import           System.IO.Error


----------- questions

data CollectionInfo = MkCollectionInfo {_ciName :: String, _ciQuestionCount :: Int}
  deriving(Eq,Show)

$(deriveJSON defaultOptions ''CollectionInfo)

data Question = Question { qsId :: String, imgPath :: FilePath, question :: Text}
              deriving (Eq, Show)

$(deriveJSON defaultOptions ''Question)


data ResourceError = FileSystemError String
                   | InvalidResourceStructure String
                   | OtherIOError IOError
                   deriving (Show, Eq)


adjustQuestionPaths :: String -> Question -> Question
adjustQuestionPaths srvDir (Question n fp q) = Question n nfp q
  where nfp = joinPath $ srvDir : reverse (take 2 $ reverse $ splitPath fp)


loadQuestionsIO :: FilePath -> IO ([ResourceError], [Question])
loadQuestionsIO = fmap partitionEithers . join . fmap (either resourceDirError readQuestions) . readResourceDir
  where

    resourceDirError :: ResourceError -> IO [Either ResourceError Question]
    resourceDirError err = return [Left err]

    -- readQuestion :: FilePath -> IO (Either ResourceError Question)
    -- readQuestion fp = do
    --   let imgp = fp -<.> "png"
    --   doesFileExist imgp >>= bool
    --      (return $ Left $ FileSystemError ("Image path: " ++ show imgp ++ " doesn't exist."))
    --      (return $ Right (Question (takeBaseName fp) imgp (pack $ takeBaseName fp)))

    readQuestion :: FilePath -> IO (Either ResourceError Question)
    readQuestion fp = do
      let ansp = fp -<.> "txt"
      let imgps = fp -<..> ["png","jpg","jpeg"]
      doesFileExist ansp >>= bool
        (return $ Left $ FileSystemError ("Answer path: " ++ show ansp ++ " doesn't exist."))
        (firstExisting imgps >>= maybe
            (return $ Left $ FileSystemError ("None of image paths: " ++ show imgps ++ " exist."))
            (\imgp -> do
              putStrLn imgp
              ans <- readFile ansp
              return $ Right (Question (takeBaseName fp) imgp ans)))
      where
        fp' -<..> exts = map (\ext -> fp' -<.> ext) exts

        firstExisting :: [String] -> IO (Maybe String)
        firstExisting = foldM fstTrue Nothing

        fstTrue Nothing fp' = bool Nothing (Just fp') <$> doesFileExist fp'
        fstTrue ret _ = return ret


    readResourceDir :: FilePath -> IO (Either ResourceError [FilePath])
    readResourceDir p = doesDirectoryExist p >>= \case
       True -> fmap (either (Left . OtherIOError) Right)
                    (tryIOError $ map (p </>) <$> listDirectory p)
       False -> return $ Left $ FileSystemError ("Path " ++ show p ++ " doesn't exist.")

    readQuestions :: [FilePath] -> IO [Either ResourceError Question]
    readQuestions = mapM (readQuestion . head) . groupBy ((==) `on` takeBaseName) . sort

----------- test static api

type ResourcesAPI =
  "static" :> Raw
  :<|>
  "collections" :> Get '[JSON] [CollectionInfo]
  :<|>
  "questions" :> Capture "collection" String :> Get '[JSON] [Question]

resourceApi :: Proxy ResourcesAPI
resourceApi = Proxy

serveQuestionData :: FilePath -> Server ResourcesAPI
serveQuestionData fp' = serveDirectory collectionDir :<|> serveCollectionList collectionDir :<|> serveQuestions collectionDir
  where
    collectionDir = joinPath [fp', "collections"]

    serveCollectionList :: FilePath -> Handler [CollectionInfo]
    serveCollectionList fp = liftIO $ do
      collections <- join $ filterM (isLegitDirectory fp) <$> listDirectory fp
      questioncount <- mapM (\newFp -> length . snd <$> loadQuestionsIO (joinPath[fp, newFp])) collections
      return $ zipWith MkCollectionInfo collections questioncount

    serveQuestions :: FilePath -> String -> Handler [Question]
    serveQuestions fp coll = liftIO $ do
      putStrLn ("requested collection: " ++ coll ++ " at " ++ fp)
      collections <- join $ filterM (isLegitDirectory fp) <$> listDirectory fp
      case coll `elemIndex` collections of
        Nothing -> putStrLn "Nothing left" *> return []
        _       -> do
          (_, questions) <- loadQuestionsIO (joinPath [fp, coll])
          print questions
          return $ map (adjustQuestionPaths "static") questions

    isLegitDirectory :: FilePath -> FilePath -> IO Bool
    isLegitDirectory root dp = (not ("." `isPrefixOf` dp) &&) <$> doesDirectoryExist (joinPath [root,dp])

startApp :: FilePath -> IO ()
-- startApp = run 8080 app
startApp fp = run 8666 (appServeStatic fp) <* putStrLn "Server started."

appServeStatic :: FilePath -> Application
appServeStatic = serve resourceApi . serveQuestionData

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''User)

-- type API = "users" :> Get '[JSON] [User]


-- app :: Application
-- app = serve api server

-- api :: Proxy API
-- api = Proxy

-- server :: Server API
-- server = return users

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]


-- loadQuestionsIO :: FilePath -> IO ([ResourceError], [Question])
-- loadQuestionsIO = fmap partitionEithers . join . fmap (either resourceDirError readQuestions) . readResourceDir
--   where

--     resourceDirError :: ResourceError -> IO [Either ResourceError Question]
--     resourceDirError err = return [Left err]

--     readQuestion :: FilePath -> IO (Either ResourceError Question)
--     readQuestion fp = do
--       let imgp = fp -<.> "png"
--       doesFileExist imgp >>= bool
--          (return $ Left $ FileSystemError ("Image path: " ++ show imgp ++ " doesn't exist."))
--          (return $ Right (Question (takeBaseName fp) imgp (pack $ takeBaseName fp)))

--     readResourceDir :: FilePath -> IO (Either ResourceError [FilePath])
--     readResourceDir p = doesDirectoryExist p >>= \case
--        True -> fmap (either (Left . OtherIOError) Right)
--                     (tryIOError $ onlyFiles $ map (p </>) <$> listDirectory p)
--        False -> return $ Left $ FileSystemError ("Path " ++ show p ++ " doesn't exist.")

--       where
--         onlyFiles = join . fmap (filterM doesFileExist)

--     readQuestions :: [FilePath] -> IO [Either ResourceError Question]
--     readQuestions = mapM (readQuestion . head) . groupBy ((==) `on` takeBaseName) . sort
