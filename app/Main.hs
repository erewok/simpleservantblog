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
import           Web.Users.Types                         (UserStorageBackend (..))


import qualified Api                                     as A
import qualified Config                                  as C


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
    Nothing  -> pure C.Production
    Just env -> pure (read env :: C.Environment)
  rs <- mkRandomSource drgNew 1000
  sk <- mkServerKey 24 Nothing
  pool <- C.makePool
  -- let cfg = C.Config { C.getPool = pool
  --                    , C.getEnv =  environment}
  let logger = C.setLogger environment
  cookieSettings <- case environment of
    C.Local -> pure localCookieSettings
    _       -> def  -- From Default Library, uses Default instance

  withResource pool housekeepBackend -- Housekeeping: eliminate old sessions
  let app = if environment == C.Local then A.withAssetsApp else A.withoutAssetsApp
  putStrLn "SimpleServantBlog up and ready to accept requests"
  app pool cookieSettings rs sk >>= Warp.run port <$> logger
