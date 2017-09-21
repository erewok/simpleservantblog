{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Api
    ( PostApi
    , adminProxyApi
    , postApi
    , createAllTables
    , withAssets
    , withAssetsApp
    , withoutAssets
    , withoutAssetsApp
    , withoutAssetsServer
    ) where


import           Control.Lens
import           Control.Monad.IO.Class                  (MonadIO, liftIO)
import           Control.Monad.Reader                    (ReaderT, lift)
import           Data.Pool                               (Pool)
import           Data.Proxy
import           Database.PostgreSQL.Simple              hiding ((:.))
import           Network.Wai                             (Application, Request)
import           Network.Wai.MakeAssets
import           Servant
import           Servant.Server.Experimental.Auth        (AuthHandler)
import           Servant.Server.Experimental.Auth.Cookie
import           Web.Users.Types                         (UserStorageBackend (..))

import           Api.Admin
import           Api.Login
import           Api.Post
import           Admin
import           Config
import           Html.Contact                            (ContactApi, contactHandlers)
import           Html.Home                               (HomePage, homePageHandlers,
                                                          BlogMain, blogMainHandlers,
                                                          AboutPage, aboutPageHandlers)
import           Html.Projects                           (ProjectsApi, projectsHandlers)
import           Models.Author                           (createAuthorTable)
import           Models.Media                            (createPostMediaTable)
import           Models.Post                             (createPostTable,
                                                          createSeriesTable)
import           Types



-- This one is separate so elm can generate for it
type WithHtml = HomePage
                :<|> "posts" :> BlogMain
                :<|> "about" :> AboutPage
                :<|> ContactApi
                :<|> ProjectsApi
                :<|> PostApi
                :<|> AdminHtml

apiHandlers :: ServerT WithHtml SimpleHandler
apiHandlers = homePageHandlers
              :<|> blogMainHandlers
              :<|> aboutPageHandlers
              :<|> contactHandlers
              :<|> projectsHandlers
              :<|> postHandlers
              :<|> adminHtmlHandlers

-- Used in production
type WithoutAssets =  WithHtml
                    :<|> LoginApi
                    :<|> AdminBackend

withoutAssetsHandlers :: ServerT WithoutAssets SimpleHandler
withoutAssetsHandlers = apiHandlers :<|> loginHandlers :<|> adminBackendHandlers

withoutAssetsServer :: SimpleApp -> Server WithoutAssets
withoutAssetsServer simpleApp = enter (simpleHandlerToHandler simpleApp) withoutAssetsHandlers

withoutAssetsApp :: SimpleApp -> IO Application
withoutAssetsApp simpleApp = do
  let context = getContext simpleApp
  return $ serveWithContext withoutAssets context (withoutAssetsServer simpleApp)


-- Used in development
type WithAssets =  WithoutAssets :<|> "assets" :> Raw

withAssetsServer :: SimpleApp -> Server WithAssets
withAssetsServer simpleApp = withAssetsHandlers :<|> serveDirectoryFileServer "assets"
  where withAssetsHandlers = withoutAssetsServer simpleApp

withAssetsApp :: SimpleApp -> IO Application
withAssetsApp simpleApp = do
  let context = getContext simpleApp
  return $ serveWithContext withAssets context (withAssetsServer simpleApp)


-- Auxiliary, helper things
getContext :: SimpleApp -> Servant.Context '[AuthHandler Request (WithMetadata Username)]
getContext simpleApp = (defaultAuthHandler settings key :: AuthHandler Request (WithMetadata Username)) :. EmptyContext
  where settings = simpleApp ^. getConfig ^. getSecurity ^. cookieSettings
        key = simpleApp ^. getConfig ^. getSecurity ^. serverKey


createAllTables :: Connection -> IO ()
createAllTables conn = initUserBackend conn >>
    execute_ conn createAuthorTable >>
      execute_ conn createSeriesTable >>
        execute_ conn createPostTable >>
          execute_ conn createPostMediaTable >>
            return ()

postApi :: Proxy PostApi
postApi = Proxy

adminBackendProxyApi :: Proxy AdminBackend
adminBackendProxyApi = Proxy

adminProxyApi :: Proxy AdminApi
adminProxyApi = Proxy

withAssets :: Proxy WithAssets
withAssets = Proxy

withoutAssets :: Proxy WithoutAssets
withoutAssets = Proxy
