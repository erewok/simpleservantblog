{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api
    ( BlogApi
    , blogApi
    , withAssets
    , withAssetsApp
    ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class             (liftIO)
import           Data.Maybe
import           Data.Pool                          (Pool)
import           Data.Proxy
import           Data.Text

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (fromRow)

import           Servant

import           Network.Wai
import           Network.Wai.Handler.Warp           as Warp
import           Network.Wai.MakeAssets

import           Api.Admin
import           Api.Post
import           Api.User
import           Html.Home
import           Html.About


-- This one is separate so elm can generate for it
type BlogApi = PostApi :<|> UserApi -- :<|> AdminApi
type WithHtml = HomePage :<|> "about" :> AboutPage :<|> BlogApi
type WithAssets =  WithHtml :<|> "assets" :> Raw

apihandlers conn = homePage
                  :<|> aboutPage
                  :<|> postHandlers conn
                  :<|> userHandlers conn
                  -- :<|> adminHandlers conn

withAssetsApp :: Pool Connection -> IO Application
withAssetsApp conn =
  serve withAssets <$> withAssetsServer conn

withAssetsServer :: Pool Connection -> IO (Server WithAssets)
withAssetsServer conn = do
  assets <- serveAssets
  return $ apihandlers conn :<|> assets

blogApi :: Proxy BlogApi
blogApi = Proxy

withHtml :: Proxy WithHtml
withHtml = Proxy

withAssets :: Proxy WithAssets
withAssets = Proxy
