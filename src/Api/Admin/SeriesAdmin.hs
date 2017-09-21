{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

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
import           Servant.Server.Experimental.Auth.Cookie

import           Api.Errors                              (appJson404)
import           Api.Login                               (Username (..))
import           Api.Types                               (ResultResp(..))
import qualified Models.Post                             as Post
import           Types

type SeriesAdminApi = "admin" :> "series" :> ReqBody '[JSON] Post.BlogSeries :> AuthProtect "cookie-auth" :> Post '[JSON] Post.BlogSeries
                      :<|> "admin" :> "series" :> Capture "id" Int :> ReqBody '[JSON] Post.BlogSeries :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
                      :<|> "admin" :> "series" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

seriesAdminHandlers :: ServerT SeriesAdminApi SimpleHandler
seriesAdminHandlers = blogSeriesAddH
                      :<|> blogSeriesUpdateH
                      :<|> blogSeriesDeleteH

blogSeriesAddH :: Post.BlogSeries -> WithMetadata Username -> SimpleHandler Post.BlogSeries
blogSeriesAddH newSeries uname = runHandlerDbHelper $ \conn -> do
  let q = "insert into series (name, description, parentid) values (?, ?, ?) returning id"
  result <- liftIO $ query conn q (Post.name newSeries
                                  , Post.description newSeries
                                  , Post.parentid newSeries) :: SimpleHandler [Only Int]
  case result of
    []    -> throwError err404
    (sid:_) -> do
      series <- liftIO $ query conn "select * from series where id = ?" sid
      if null series then throwError err404 else return $ Prelude.head series

blogSeriesUpdateH :: Int -> Post.BlogSeries -> WithMetadata Username -> SimpleHandler ResultResp
blogSeriesUpdateH seriesId newSeries uname = runHandlerDbHelper $ \conn -> do
  let q = Query $ B.unwords ["update series set name = ?, description = ?, parentid = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (Post.name newSeries
                                   , Post.description newSeries
                                   , Post.parentid newSeries
                                   , seriesId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "series updated"

blogSeriesDeleteH :: Int -> WithMetadata Username -> SimpleHandler ResultResp
blogSeriesDeleteH seriesId uname = runHandlerDbHelper $ \conn -> do
  let q = "delete CASCADE from series where id = ?"
  result <- liftIO $ execute conn q (Only seriesId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "series and posts deleted"
