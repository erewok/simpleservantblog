module PostsSpec where

import           Control.Exception          (throwIO)
import           Control.Monad.Trans.Except
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
import           Test.Hspec

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


spec :: Pool Connection -> Spec
spec conn = do
  describe "/post" $ do
    withClient (liftIO mkApp conn) $ do
      it "get a list of post Overviews" $ \ host -> do
        try host getPostOs `shouldReturn` []

      it "allows to show items by id" $ \ host -> do
        try host (getPostById 0) `shouldReturn` Item 0 "example item"

      it "throws a 404 for missing post" $ \ host -> do
        try host (getPostById 42) `shouldThrow` (\ e -> responseStatus e == notFound404)

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ manager -> do
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (manager, baseUrl)

type Host = (Manager, BaseUrl)

try :: Host -> (Manager -> BaseUrl -> ClientM a) -> IO a
try (manager, baseUrl) action = either throwIO return =<<
  runExceptT (action manager baseUrl)
