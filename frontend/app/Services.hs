{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Services where

import           Control.Lens                 hiding (Identity)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.RWS
import           Data.Singletons.TH
import           Data.Vinyl

import           Control.Category
import           Prelude                      hiding (id, (.))
import qualified Prelude                      as P

--
import Resources
import ElectronApi


------ Intepreter type ----------

newtype Interpret c d = Interpret {unInterpret :: forall n a. d n => (forall m. c m => m a) -> n a}
newtype Interp g eff = Interp {unInterp :: forall a. (forall f. eff f => f a) -> g a}

newtype ServiceFunX c r = ServiceFunX { unServiceFunx :: forall m. c m => m r}

instance Category Interpret where
  id = Interpret P.id
  Interpret f . Interpret g = Interpret $ \h -> f (g h)

------ Effects interfaces ------

type Url = String

class Monad m => MonadHttp m where
  httpGet :: String -> m String

class Monad m => MonadLog m where
  logM :: String -> m ()

class Monad m => MonadRestApi m where
  getUsersIds :: m [Int]

------ Application monad & services

data Service = LoggingService | ResourceService | ElectronApiService

type family ServicesFam m (s :: Service) :: * where
  ServicesFam g 'LoggingService = Interpret MonadLog g
  ServicesFam g 'ResourceService = Interpret MonadResource g
  ServicesFam g 'ElectronApiService = Interpret MonadElectronApi g

type family ServiceMonadFam (s :: Service) r :: * where
  ServiceMonadFam 'ResourceService r = ServiceFunX MonadResource r
  ServiceMonadFam 'LoggingService  r = ServiceFunX MonadLog r


newtype Attr f g = Attr { _unAttr :: ServicesFam f g}


(=::) :: (sing f -> ServicesFam g f -> Attr g f)
(=::) _ = Attr

makeLenses ''Attr
genSingletons [ ''Service ]

type Services eff = Rec (Attr eff) '[ 'LoggingService, 'ResourceService, 'ElectronApiService]


----- Application environment mock

type MockMonad r w = RWS r w ()


------ Effects implementations ------

instance MonadLog IO where
  logM = putStrLn

productionResourceService :: Interpret MonadResource MonadIO
productionResourceService = Interpret liftIO

productionLogService :: Interpret MonadLog MonadIO
productionLogService = Interpret liftIO

productionElectronApiService :: Interpret MonadElectronApi MonadIO
productionElectronApiService = Interpret liftIO

---- application services implementations

runApp :: forall c m a. (Monad m, c m) => App a -> Services c -> m a
runApp app = runReaderT (unApp app)

newtype App a = App { unApp :: forall c m. (c m, Monad m ) => ReaderT (Services c) m a  }

type Application a = forall c m. (c m, Monad m) => ReaderT (Services c) m a
type ServiceFun1 s r = forall c m ss. (c m, Monad m, s ∈ ss) => ReaderT (Rec (Attr c) ss) m r
type ServiceFun2 s1 s2 r = forall c m ss. (c m, Monad m, s1 ∈ ss, s2 ∈ ss) => ReaderT (Rec (Attr c) ss) m r

---- services packs

productionServices :: Services MonadIO
productionServices = (SLoggingService =:: productionLogService)
                  :& (SResourceService =:: productionResourceService)
                  :& (SElectronApiService =:: productionElectronApiService) :& RNil


---- auxiliary :call with service functions:

callWithResourceService :: forall c m ss r. (c m, Monad m, 'ResourceService ∈ ss) => (forall n. MonadResource n => n r) -> ReaderT (Rec (Attr c) ss) m r
callWithResourceService sf = do
  rss <- view (rlens SResourceService) <$> ask
  lift $ f . _unAttr $ rss
    where
      f :: Interpret MonadResource c -> m r
      f (Interpret run) = run sf

callWithLogService :: forall c m ss r. (c m, Monad m, 'LoggingService ∈ ss) => (forall n. MonadLog n => n r) -> ReaderT (Rec (Attr c) ss) m r
callWithLogService sf = do
  rss <- view (rlens SLoggingService) <$> ask
  lift $ f . _unAttr $ rss
    where
      f :: Interpret MonadLog c -> m r
      f (Interpret run) = run sf

callWithElectronApiService :: forall c m ss r. (c m, Monad m, 'ElectronApiService ∈ ss) => (forall n. MonadElectronApi n => n r) -> ReaderT (Rec (Attr c) ss) m r
callWithElectronApiService sf = do
  rss <- view (rlens SElectronApiService) <$> ask
  lift $ f . _unAttr $ rss
    where
      f :: Interpret MonadElectronApi c -> m r
      f (Interpret run) = run sf


---- usage example
fun3 :: ServiceFun2 'LoggingService 'ResourceService ()
fun3 = do
  questions <- callWithResourceService $ loadQuestions "test_resources"
  callWithLogService $ logM (show questions)



app1 :: App ()
app1 = App fun3

test3 :: IO ()
test3 = void $ runApp app1 (rcast productionServices)


