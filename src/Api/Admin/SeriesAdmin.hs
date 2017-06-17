{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api.Admin.SeriesAdmin
  ( SeriesAdminApi
  , seriesAdminHandlers
  ) where


import           Control.Monad.IO.Class                  (liftIO)
import qualified Data.ByteString.Char8                   as B
import           Data.Pool                               (Pool, withResource)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types        (Query (..))
import qualified Data.Text                               as T
import           Data.Time                               (getCurrentTime)
import           Servant

import           Api.Errors                              (appJson404)
import           Api.Types                               (ResultResp(..))
import qualified Models.Post                             as Post


type SeriesAdminApi = "admin" :> "series" :> ReqBody '[JSON] Post.BlogSeries :> AuthProtect "cookie-auth" :> Post '[JSON] Post.BlogSeries
                      :<|> "admin" :> "series" :> Capture "id" Int :> ReqBody '[JSON] Post.BlogSeries :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
                      :<|> "admin" :> "series" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

seriesAdminHandlers :: Pool Connection -> Server SeriesAdminApi
seriesAdminHandlers conn = blogSeriesAddH
                           :<|> blogSeriesUpdateH
                           :<|> blogSeriesDeleteH
  where blogSeriesAddH newSeries _ = go $ addSeries newSeries
        blogSeriesUpdateH seriesId series _ = go $ updateSeries seriesId series
        blogSeriesDeleteH seriesId _ = go $ deleteSeries seriesId
        go = withResource conn


addSeries :: Post.BlogSeries -> Connection -> Handler Post.BlogSeries
addSeries newSeries conn = do
  let q = "insert into series (name, description, parentid) values (?, ?, ?) returning id"
  result <- liftIO $ query conn q (Post.name newSeries
                                  , Post.description newSeries
                                  , Post.parentid newSeries) :: Handler [Only Int]
  case result of
    []    -> throwError err404
    (sid:_) -> do
      series <- liftIO $ query conn "select * from series where id = ?" sid
      if null series then throwError err404 else return $ Prelude.head series

updateSeries :: Int -> Post.BlogSeries -> Connection -> Handler ResultResp
updateSeries seriesId newSeries conn = do
  let q = Query $ B.unwords ["update series set name = ?, description = ?, parentid = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (Post.name newSeries
                                   , Post.description newSeries
                                   , Post.parentid newSeries
                                   , seriesId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "series updated"

deleteSeries :: Int -> Connection -> Handler ResultResp
deleteSeries seriesId conn = do
  let q = "delete CASCADE from series where id = ?"
  result <- liftIO $ execute conn q (Only seriesId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "series and posts deleted"
