{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Crypto.Cipher.AES                       (AES256)
import           Crypto.Cipher.Types
import           Crypto.Hash.Algorithms                  (SHA256)
import           Crypto.Random                           (drgNew)
import           Data.Default
import           Data.Pool                               (withResource)
import           Data.Proxy
import           Network.Wai.Handler.Warp                as Warp
import           Servant.Server.Experimental.Auth.Cookie
import           System.Environment                      (lookupEnv)
import           System.Log.FastLogger                   (defaultBufSize, withFastLogger, LogType(..))
import           Web.Users.Types                         (UserStorageBackend (..))


import qualified Api                                     as A
import           Config


-- This is just for testing in a local Environment, so we don't have to run
-- HTTPS. Chrome and other browsers won't store a cookie with "Secure" over HTTP.
localCookieSettings :: AuthCookieSettings
localCookieSettings = AuthCookieSettings
    { acsSessionField = "Session"
    , acsCookieFlags  = []  -- Normally we'd want "HttpOnly" and "Secure" here.
    , acsMaxAge       = fromIntegral (12 * 3600 :: Integer) -- 12 hours
    , acsExpirationFormat = "%0Y%m%d%H%M%S"
    , acsPath         = "/"
    , acsHashAlgorithm = Proxy :: Proxy SHA256
    , acsCipher       = Proxy :: Proxy AES256
    , acsEncryptAlgorithm = ctrCombine
    , acsDecryptAlgorithm = ctrCombine }


main :: IO ()
main = do
  port' <- lookupEnv "SERVER_PORT"
  port <- case port' of
    Nothing -> pure 9005
    Just p  -> pure $ read p
  environ <- lookupEnv "ENVIRONMENT"
  environment <- case environ of
    Nothing  -> pure Production
    Just env -> pure (read env :: Environment)
  rs <- mkRandomSource drgNew 1000
  sk <-  generateRandomBytes 24
  let key = mkPersistentServerKey sk
  pool <- makePool

  cookieSettings <- case environment of
    Local -> pure localCookieSettings
    _       -> def  -- From Default Library, uses Default instance

  let stdErrLog = withFastLogger (LogStderr defaultBufSize)
  let stdOutLog = withFastLogger (LogStdout defaultBufSize)
  let loggers = AppLoggers { _stdErrLogger = stdErrLog
                           , _stdOutLogger = stdOutLog }
  let security = AppSecurity {
        _cookieSettings = cookieSettings
        , _randomSource = rs
        , _serverKey = key }
  let cfg = SimpleAppConfig { _getEnv =  environment
                            , _getSecurity = security }
  let simpleApp = SimpleApp { _getPool = pool
                            , _getConfig = cfg
                            , _getLoggers = loggers }
  withResource pool housekeepBackend -- Housekeeping: eliminate old sessions
  let app = if environment == Local then A.withAssetsApp else A.withoutAssetsApp
  putStrLn $ "SimpleServantBlog up on port " ++ show port ++ " and ready to accept requests"
  app simpleApp >>= Warp.run port <$> setRequestLogger environment
