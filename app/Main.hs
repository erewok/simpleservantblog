module Main where

import           Control.Exception        (throwIO)
import           Data.Pool                (withResource)
import           Network.Wai.Handler.Warp as Warp
import           System.Environment       (lookupEnv)

import qualified Api                      as A
import qualified Config                   as C


main :: IO ()
main = do
  port' <- lookupEnv "SERVER_PORT"
  port <- case port' of
    Nothing -> pure 9005
    Just p  -> pure $ read p
  environ <- lookupEnv "ENVIRONMENT"
  environment <- case environ of
    Nothing  -> pure C.Local
    Just env -> pure (read env :: C.Environment)

  pool <- C.makePool
  let cfg = C.Config { C.getPool = pool
                     , C.getEnv =  environment}
      logger = C.setLogger environment
  Warp.run port $ logger $ A.app pool
