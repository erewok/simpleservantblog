{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Post
    (
    PostApi
    , postHandlers
    ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class             (liftIO)
import           Data.Maybe
import           Data.Proxy
import           Data.Text

import           Data.Pool                          (withResource)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (fromRow)

import           Servant

import           Network.Wai
import           Network.Wai.Handler.Warp           as Warp

import           Models.Post


type PostApi = "post" :> Get '[JSON] [PostOverview]
  :<|> "post" :> Capture "id" Int  :> Get  '[JSON] BlogPost
  :<|> "post" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
  :<|> "post" :> "series" :> Capture "id" Int  :> Get  '[JSON] [BlogPost]
  :<|> "series" :> Capture "id" Int  :> Get  '[JSON] BlogSeries

postHandlers conn = blogPostListH
              :<|> blogPostDetailH
              :<|> blogPostAddH
              :<|> blogPostSeriesH
              :<|> blogSeriesH
  where blogPostListH = withResource conn listPosts
        blogPostDetailH postId = withResource conn $ flip getPost postId
        blogPostAddH newPost = withResource conn $ flip addPost newPost
        blogPostSeriesH seriesId = withResource conn $ flip getPostSeries seriesId
        blogSeriesH seriesId = withResource conn $ flip getSeries seriesId

listPosts :: Connection -> Handler [PostOverview]
listPosts conn = liftIO $ query_ conn postOverviewAllQuery

getPost :: Connection -> Int -> Handler BlogPost
getPost conn postId = do
  let q = "select * from post where id = ? and pubdate is not null"
  result <- liftIO $ query conn q (Only postId)
  case result of
    (x:_) -> return x
    []    -> throwError err404

getPostSeries :: Connection -> Int -> Handler [BlogPost]
getPostSeries conn seriesId = liftIO $ query conn seriesPostsQuery (Only seriesId)

addPost :: Connection -> BlogPost -> Handler BlogPost
addPost conn newPost = do
  result <- liftIO $ addPost' conn newPost
  case result of
    (x:_) -> return x
    []    -> throwError err404

addPost' :: Connection -> BlogPost -> IO [BlogPost]
addPost' conn newPost = do
  let q = "insert into post values (?, ?, ?)"
  query conn q newPost

getSeries :: Connection -> Int -> Handler BlogSeries
getSeries conn seriesId = do
  let q = "select id, name, description, parentid from series where seriesid = ?"
  result <- liftIO $ query conn q (Only seriesId)
  case result of
    (x:_) -> return x
    []    -> throwError err404
