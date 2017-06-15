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
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import qualified Data.List                  as L
import           Data.Maybe
import           Data.Pool                  (Pool, withResource)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Servant

import           Api.Errors                 (appJson404)
import           Models.Post
import           Models.Media


type PostApi = "post" :> Get '[JSON] [PostOverview]
  :<|> "post" :> Capture "id" Int  :> Get  '[JSON] BlogPost
  :<|> "post" :> "media" :> Capture "id" Int  :> Get  '[JSON] [Media]
  :<|> "series" :> "post" :> Capture "id" Int  :> Get  '[JSON] PostSeries
  :<|> "series" :> Get  '[JSON] [BlogSeries]
  :<|> "series" :> Capture "id" Int  :> Get '[JSON] BlogSeries

postHandlers :: Pool Connection -> Server PostApi
postHandlers conn = blogPostListH
              :<|> blogPostDetailH
              :<|> blogPostMediaH
              :<|> blogPostSeriesH
              :<|> blogSeriesListH
              :<|> blogSeriesDetailH
  where blogPostListH = withResource conn listPosts
        blogPostDetailH postId = withResource conn $ flip getPost postId
        blogPostMediaH postId = withResource conn $ flip getPostMedia postId
        blogPostSeriesH postId = withResource conn $ flip getPostWithSeries postId
        blogSeriesListH = withResource conn listSeries
        blogSeriesDetailH seriesId = withResource conn $ flip getSeries seriesId

listPosts :: Connection -> Handler [PostOverview]
listPosts conn = liftIO $ query_ conn postOverviewAllQuery


getPost :: Connection -> Int -> Handler BlogPost
getPost conn postId = do
  let q = "select * from post where id = ? and pubdate is not null"
  result <- liftIO $ query conn q (Only postId)
  case result of
    (x:_) -> return x
    []    -> throwError $ appJson404 "Post not found"

getPostMedia :: Connection -> Int -> Handler [Media]
getPostMedia conn postId = liftIO $ query conn postMediaQueryByPostId (Only postId)

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

prevCurrNextPost :: Int -> [BlogPost] -> ([BlogPost], Maybe BlogPost, [BlogPost])
prevCurrNextPost postId posts
  | isNothing findPost = ([], Nothing, [])
  | otherwise = (prev, findPost, next)
  where findPost = L.find (\p -> bid p == postId) posts
        post = fromJust findPost
        prev = filter (\p -> ordinal p < ordinal post) posts
        next = filter (\p -> ordinal p > ordinal post) posts

listSeries :: Connection -> Handler [BlogSeries]
listSeries conn = liftIO $ query_ conn "select * from series"

getSeries :: Connection -> Int -> Handler BlogSeries
getSeries conn seriesId = do
  let q = "select * from series where id = ?"
  res <- liftIO $ query conn q (Only seriesId)
  case res of
    (x:_) -> return x
    _     -> throwError $ appJson404 "Series not found"
