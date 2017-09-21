{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import qualified Control.Exception as Exc
import Control.Lens
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Data.Monoid ((<>))
import Data.Pool
import Database.PostgreSQL.Simple
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Servant.Server.Experimental.Auth.Cookie
import System.Environment (lookupEnv)
import System.Log.FastLogger (FastLogger)


data Environment
  = Local
  | Dev
  | Staging
  | Test
  | Production
  deriving (Eq, Show, Read, Enum, Ord)

data AppSecurity = AppSecurity
  { _cookieSettings :: AuthCookieSettings
  , _randomSource :: RandomSource
  , _serverKey :: PersistentServerKey
  }

makeLenses ''AppSecurity

data SimpleAppConfig = SimpleAppConfig
  { _getEnv :: Environment
  , _getSecurity :: AppSecurity
  }

makeLenses ''SimpleAppConfig

--
data AppLoggers = AppLoggers
  { _stdErrLogger :: (FastLogger -> IO ()) -> IO ()
  , _stdOutLogger :: (FastLogger -> IO ()) -> IO ()
  }

makeLenses ''AppLoggers

-- The primary repository for all app configuration
-- to be housed inside a  ReaderT
data SimpleApp = SimpleApp
  { _getPool :: Pool Connection
  , _getConfig :: SimpleAppConfig
  , _getLoggers :: AppLoggers
  }

makeLenses ''SimpleApp

-- | This returns a 'Middleware' based on the environment that we're in.
setRequestLogger :: Environment -> Middleware
setRequestLogger Test = id
setRequestLogger Local = logStdoutDev
setRequestLogger _ = logStdout

makeConnString :: IO (Maybe BS.ByteString)
makeConnString =
  runMaybeT $ do
    let keys = ["host=", " port=", " user=", " password=", " dbname="]
        envs = ["DB_HOST", "DB_PORT", "DB_USER", "DB_PASSWD", "DB_NAME"]
    envVars <- traverse (MaybeT . lookupEnv) envs
    return $ mconcat . zipWith (<>) keys $ BS.pack <$> envVars

-- see: http://codeundreamedof.blogspot.com/2015/01/a-connection-pool-for-postgresql-in.html
makePool :: IO (Pool Connection)
makePool = do
  connString <- makeConnString
  case connString of
    Nothing ->
      Exc.throwIO
        (userError "Database Configuration not present in environment.")
    Just info -> createPool (connectPostgreSQL info) close 1 10 10
