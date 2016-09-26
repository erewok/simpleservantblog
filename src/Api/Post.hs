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
import qualified Data.List                          as L
import           Data.Maybe
import           Data.Pool                          (withResource)
import           Data.Proxy
import           Data.Text                          hiding (dropWhile, filter,
                                                     head, takeWhile)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (fromRow)

import           Servant

import           Network.Wai
import           Network.Wai.Handler.Warp           as Warp

import           Models.Post


type PostApi = "post" :> Get '[JSON] [PostOverview]
  :<|> "post" :> Capture "id" Int  :> Get  '[JSON] BlogPost
  :<|> "post" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
  :<|> "series" :> "post" :> Capture "id" Int  :> Get  '[JSON] PostSeries

postHandlers conn = blogPostListH
              :<|> blogPostDetailH
              :<|> blogPostAddH
              :<|> blogPostSeriesH
  where blogPostListH = withResource conn listPosts
        blogPostDetailH postId = withResource conn $ flip getPost postId
        blogPostAddH newPost = withResource conn $ flip addPost newPost
        blogPostSeriesH postId = withResource conn $ flip getPostWithSeries postId

listPosts :: Connection -> Handler [PostOverview]
listPosts conn = liftIO $ query_ conn postOverviewAllQuery

getPost :: Connection -> Int -> Handler BlogPost
getPost conn postId = do
  let q = "select * from post where id = ? and pubdate is not null"
  result <- liftIO $ query conn q (Only postId)
  case result of
    (x:_) -> return x
    []    -> throwError err404

getPostWithSeries :: Connection -> Int -> Handler PostSeries
getPostWithSeries conn postId = do
  posts <- liftIO $ query conn seriesPostsQuery (Only postId)
  let (prev, current, next) = prevCurrNextPost postId posts
  case current of
    Nothing -> throwError err404
    Just post -> do
      let seriesq = "select id, name, description, parentid from series where id = ?"
      series <- liftIO $ query conn seriesq (Only $ fromJust $ seriesId post)
      return $ PostSeries prev post next $ head series

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

prevCurrNextPost :: Int -> [BlogPost] -> ([BlogPost], Maybe BlogPost, [BlogPost])
prevCurrNextPost postId posts
  | isNothing findPost = ([], Nothing, [])
  | otherwise = (prev, findPost, next)
  where findPost = L.find (\p -> bid p == postId) posts
        post = fromJust findPost
        prev = filter (\p -> ordinal p < ordinal post) posts
        next = filter (\p -> ordinal p > ordinal post) posts
