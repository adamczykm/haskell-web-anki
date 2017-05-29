{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UnicodeSyntax              #-}

module ReflexDomApp where

import           Control.Monad.Reader
import           Services
import           Data.Vinyl

import           Reflex.Dom

newtype ReflexDomApp a = ReflexDomApp { unReflexDomApp :: forall c t m. (c m, Monad m, MonadWidget t m) => ReaderT (Services c) m a  }


type ReflexServiceFun s m r = forall c ss. (c m, s ∈ ss) => ReaderT (Rec (Attr c) ss) m r
type ReflexServiceFun2 s1 s2 m r = forall c ss. (c m, s1 ∈ ss, s2 ∈ ss) => ReaderT (Rec (Attr c) ss) m r
type ReflexServiceFun3 s1 s2 s3 m r = forall c ss. (c m, s1 ∈ ss, s2 ∈ ss, s3 ∈ ss) => ReaderT (Rec (Attr c) ss) m r

runReflexDomApp :: ReflexDomApp a
                -> Services MonadIO
                -> IO ()
runReflexDomApp app srvcs = mainWidget $ void $ (runReaderT . unReflexDomApp) app srvcs
