{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api
    ( BlogApi
    , adminProxyApi
    , blogApi
    , withAssets
    , withAssetsApp
    ) where


import           Data.Pool                               (Pool)
import           Data.Proxy
import           Database.PostgreSQL.Simple              hiding ((:.))
import           Network.Wai
import           Network.Wai.MakeAssets
import           Servant
import           Servant.Server.Experimental.Auth        (AuthHandler)
import           Servant.Server.Experimental.Auth.Cookie

import           Api.Admin.Admin
import           Api.Admin.Login
import           Api.Post
import           Api.User
import           Html.About
import           Html.Home


-- This one is separate so elm can generate for it
type BlogApi = PostApi :<|> UserApi
type WithHtml = HomePage
                :<|> "about" :> AboutPage
                :<|> BlogApi
type WithAssets =  WithHtml
                  :<|> LoginApi
                  :<|> AdminBackend
                  :<|> "assets" :> Raw

apihandlers conn = homePage
                  :<|> aboutPage
                  :<|> postHandlers conn
                  :<|> userHandlers conn

withAssetsApp :: Pool Connection -> AuthCookieSettings -> RandomSource -> ServerKey -> IO Application
withAssetsApp conn settings rs key = do
  let context = (defaultAuthHandler settings key :: AuthHandler Request Username) :. EmptyContext
  server <- withAssetsServer conn settings rs key
  return $ serveWithContext withAssets context server

withAssetsServer :: Pool Connection -> AuthCookieSettings -> RandomSource -> ServerKey -> IO (Server WithAssets)
withAssetsServer conn settings rs key = do
  assets <- serveAssets
  return (apihandlers conn
            :<|> loginServer conn settings rs key
            :<|> adminBackendHandlers conn
            :<|> assets)

blogApi :: Proxy BlogApi
blogApi = Proxy

adminBackendProxyApi :: Proxy AdminBackend
adminBackendProxyApi = Proxy

adminProxyApi :: Proxy AdminApi
adminProxyApi = Proxy

withHtml :: Proxy WithHtml
withHtml = Proxy

withAssets :: Proxy WithAssets
withAssets = Proxy
