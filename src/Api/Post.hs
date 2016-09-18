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


type PostApi = "post" :> Get '[JSON] [BlogPost]
  :<|> "post" :> Capture "id" Int  :> Get  '[JSON] BlogPost
  :<|> "post" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
  :<|> "post" :> "series" :> Capture "id" Int  :> Get  '[JSON] BlogSeriesWithPosts

postHandlers conn = blogPostListH
              :<|> blogPostDetailH
              :<|> blogPostAddH
              :<|> blogPostSeriesH
  where blogPostListH = withResource conn listPosts
        blogPostDetailH postId = withResource conn $ flip getPost postId
        blogPostAddH newPost = withResource conn $ flip addPost newPost
        blogPostSeriesH seriesId = withResource conn $ flip getPostSeries seriesId

listPosts :: Connection -> Handler [BlogPost]
listPosts conn = do
  let q = "select * from post where pubdate is not null"
  liftIO $ query_ conn q

getPost :: Connection -> Int -> Handler BlogPost
getPost conn postId = do
  let q = "select * from post where id = ? and pubdate is not null"
  result <- liftIO $ query conn q (Only postId)
  case result of
    (x:_) -> return x
    []    -> throwError err404

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

getPostSeries :: Connection -> Int -> Handler BlogSeriesWithPosts
getPostSeries conn seriesId = do
  series' <- liftIO $ getSeries conn seriesId
  series <- case series' of
    [] -> throwError err404
    (x:_) -> pure x
  let q2 = "select ? from post where seriesid = ? order by ordinal asc"
  result <- liftIO $ query conn q2 (blogPostColumns, seriesId)
  case result of
    [] -> throwError err404
    posts -> return BlogSeriesWithPosts {bsid = sid series
                                        , sname = name series
                                        , sdescription = description series
                                        , parent = parentid series
                                        , posts = posts}

getSeries :: Connection -> Int -> IO [BlogSeries]
getSeries conn seriesId = do
  let q1 = "select id, name, description, parentid from series where seriesid = ?"
  query conn q1 (Only seriesId)
