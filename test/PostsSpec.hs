module PostsSpec where

import           Control.Exception          (throwIO)
import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class     (liftIO)
import           Crypto.Cipher.Types
import           Crypto.Random              (drgNew)
import           Data.Default
import           Data.Pool                  (Pool)
import           Data.Proxy
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple hiding ((:.))
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             newManager)
import           Network.HTTP.Types
import           Network.Wai                (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Servant.QuickCheck
import           Servant.Server.Experimental.Auth.Cookie
import           Test.Hspec
import Test.QuickCheck

import           Api
import           Api.Post
import qualified Config                     as C
import           Models.Post


getPostOs :: Manager -> BaseUrl -> ClientM [PostOverview]
getPostById :: Int -> Manager -> BaseUrl -> ClientM BlogPost
getSeriesPost :: Int -> Manager -> BaseUrl -> ClientM PostSeries
getSeries :: Manager -> BaseUrl -> ClientM [BlogSeries]
getSeriesById :: Int -> Manager -> BaseUrl -> ClientM BlogSeries
getPostOs
  :<|> getPostById
  :<|> getSeriesPost
  :<|> getSeries
  :<|> getSeriesById = client postApi


mkApp :: Pool Connection -> Application
mkApp conn = serve postApi $ postHandlers conn

wholeSiteChecks :: Pool Connection -> Spec
wholeSiteChecks conn = it "won't crash and conforms to various rules" $
    withServantServer postApi (pure $ postHandlers conn) $ \burl ->
      serverSatisfies postApi burl stdArgs
        ( not500
        <%> onlyJsonObjects
        <%> mempty)


spec :: Spec
spec = describe "Post API tests" $ do
    conn <- C.makePool
    wholeSiteChecks conn
    withClient (pure $ mkApp conn) $ do
      it "get a list of post Overviews" $ \ host ->
        try host getPostOs `shouldReturn` []
--
--       it "allows to show items by id" $ \ host ->
--         try host (getPostById 0) `shouldReturn` Item 0 "example item"
--
      it "throws a 404 for missing post" $ \ host ->
        try host (getPostById 42) `shouldThrow` (\ e -> responseStatus e == notFound404)

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient ap innerSpec =
  beforeAll (newManager defaultManagerSettings) $
    flip aroundWith innerSpec $ \ action manager ->
      testWithApplication ap $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (manager, baseUrl)

type Host = (Manager, BaseUrl)

try :: Host -> (Manager -> BaseUrl -> ClientM a) -> IO a
try (manager, baseUrl) action = either throwIO return =<<
  runExceptT (action manager baseUrl)
