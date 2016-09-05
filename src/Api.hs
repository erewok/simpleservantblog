{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api
    ( BlogApi
    , blogApi
    , withAssets
    , server
    , app
    , withAssetsApp
    ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class             (liftIO)
import           Data.Maybe
import Data.Pool (Pool)
import           Data.Proxy
import           Data.Text

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (fromRow)

import           Servant

import           Network.Wai
import           Network.Wai.Handler.Warp           as Warp
import           Network.Wai.MakeAssets

import           Api.Post
import           Api.User


type BlogApi = PostApi
              :<|> UserApi

type WithAssets = BlogApi :<|> Raw

apihandlers conn = postHandlers conn
                  :<|> userHandlers conn

withAssets :: Proxy WithAssets
withAssets = Proxy

blogApi :: Proxy BlogApi
blogApi = Proxy

server :: Pool Connection -> Server BlogApi
server = apihandlers

app :: Pool Connection -> Application
app conn = serve blogApi $ server conn

withAssetsApp :: Pool Connection -> IO Application
withAssetsApp conn =
  serve withAssets <$> withAssetsServer conn

withAssetsServer :: Pool Connection -> IO (Server WithAssets)
withAssetsServer conn = do
  assets <- serveAssets
  return $ apihandlers conn :<|> assets
