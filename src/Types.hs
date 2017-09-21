{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}


module Types
    ( SimpleHandler(..)
    , simpleHandlerToHandler
    , runHandlerDbHelper
    ) where


import           Control.Monad.Base                      (MonadBase)
import           Control.Monad.Catch                     (MonadThrow)
import           Control.Monad.Reader                    (ReaderT, ask, MonadReader, runReaderT)
import           Control.Monad.IO.Class                  (MonadIO)
import           Control.Monad.Error.Class               (MonadError)
import           Control.Monad.Trans.Control             (MonadBaseControl (..))
import           Data.Pool                               (withResource)
import           Database.PostgreSQL.Simple              (Connection)
import           Servant

import           Config


newtype SimpleHandler a = SimpleHandler {unHandler :: ReaderT SimpleApp Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SimpleApp, MonadThrow, MonadBase IO, MonadError ServantErr)


instance MonadBaseControl IO SimpleHandler where
  type StM SimpleHandler a = Either ServantErr a
  liftBaseWith f = SimpleHandler $ liftBaseWith $ \q -> f (q . unHandler)
  restoreM st = SimpleHandler (restoreM st)

simpleHandlerToHandler :: SimpleApp -> SimpleHandler :~> Handler
simpleHandlerToHandler simpleApp = NT simpleHandlerToHandler'
  where
    simpleHandlerToHandler' :: SimpleHandler a -> Handler a
    simpleHandlerToHandler' h = runReaderT (unHandler h) simpleApp

runHandlerDbHelper :: forall a. (Connection -> SimpleHandler a) -> SimpleHandler a
runHandlerDbHelper fun = do
  conn <- _getPool <$> ask
  withResource conn fun
