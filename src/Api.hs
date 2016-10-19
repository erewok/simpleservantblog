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
    , withoutAssets
    , withoutAssetsApp
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
type WithoutAssets =  WithHtml
                    :<|> LoginApi
                    :<|> AdminBackend
type WithAssets =  WithHtml
                  :<|> LoginApi
                  :<|> AdminBackend
                  :<|> "assets" :> Raw

apihandlers :: Pool Connection -> Server WithHtml
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

withoutAssetsApp :: Pool Connection -> AuthCookieSettings -> RandomSource -> ServerKey -> IO Application
withoutAssetsApp conn settings rs key = do
  let context = (defaultAuthHandler settings key :: AuthHandler Request Username) :. EmptyContext
  server <- withoutAssetsServer conn settings rs key
  return $ serveWithContext withoutAssets context server

withoutAssetsServer :: Pool Connection -> AuthCookieSettings -> RandomSource -> ServerKey -> IO (Server WithoutAssets)
withoutAssetsServer conn settings rs key = return $ apihandlers conn
                                      :<|> loginServer conn settings rs key
                                      :<|> adminBackendHandlers conn

blogApi :: Proxy BlogApi
blogApi = Proxy

adminBackendProxyApi :: Proxy AdminBackend
adminBackendProxyApi = Proxy

adminProxyApi :: Proxy AdminApi
adminProxyApi = Proxy

withAssets :: Proxy WithAssets
withAssets = Proxy

withoutAssets :: Proxy WithoutAssets
withoutAssets = Proxy
