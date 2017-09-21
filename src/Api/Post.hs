{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Post
    (
    PostApi
    , postHandlers
    ) where

import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.List                  as L
import           Data.Maybe
import           Database.PostgreSQL.Simple
import           Servant

import           Api.Errors                 (appJson404)
import           Models.Post
import           Models.Media
import           Types                      (SimpleHandler, runHandlerDbHelper)


type PostApi = "post" :> Get '[JSON] [PostOverview]
  :<|> "post" :> Capture "id" Int  :> Get  '[JSON] BlogPost
  :<|> "post" :> "media" :> Capture "id" Int  :> Get  '[JSON] [Media]
  :<|> "series" :> "post" :> Capture "id" Int  :> Get  '[JSON] PostSeries
  :<|> "series" :> Get  '[JSON] [BlogSeries]
  :<|> "series" :> Capture "id" Int  :> Get '[JSON] BlogSeries


postHandlers :: ServerT PostApi SimpleHandler
postHandlers = listPostsH
               :<|> getPostH
               :<|> getPostMediaH
               :<|> getPostWithSeriesH
               :<|> getSeriesListH
               :<|> getSeriesDetailH

listPostsH :: SimpleHandler [PostOverview]
listPostsH = runHandlerDbHelper $ \db -> liftIO $ query_ db postOverviewAllQuery

getPostH :: Int -> SimpleHandler BlogPost
getPostH postId = runHandlerDbHelper $ \db -> do
    let q = "select * from post where id = ? and pubdate is not null"
    result <- liftIO $ query db q (Only postId)
    case result of
      (x:_) -> return x
      []   -> throwError $ appJson404 "Post not found"

getPostMediaH :: Int -> SimpleHandler [Media]
getPostMediaH postId = runHandlerDbHelper $ \db -> liftIO $ query db postMediaQueryByPostId (Only postId)

getPostWithSeriesH :: Int -> SimpleHandler PostSeries
getPostWithSeriesH postId =  runHandlerDbHelper $ \db -> do
  posts <- liftIO $ query db seriesPostsQuery (Only postId)
  let (prev, current, next) = prevCurrNextPost postId posts
  case current of
    Nothing -> throwError err404
    Just post -> do
      let seriesq = "select id, name, description, parentid from series where id = ?"
      series <- liftIO $ query db seriesq (Only $ fromJust $ seriesId post)
      return $ PostSeries prev post next $ head series

getSeriesListH :: SimpleHandler [BlogSeries]
getSeriesListH = runHandlerDbHelper $ \db -> liftIO $ query_ db "select * from series"

getSeriesDetailH :: Int -> SimpleHandler BlogSeries
getSeriesDetailH seriesId = runHandlerDbHelper $ \db -> do
  let q = "select * from series where id = ?"
  res <- liftIO $ query db q (Only seriesId)
  case res of
    (x:_) -> return x
    _     -> throwError $ appJson404 "Series not found"

prevCurrNextPost :: Int -> [BlogPost] -> ([BlogPost], Maybe BlogPost, [BlogPost])
prevCurrNextPost postId posts
  | isNothing findPost = ([], Nothing, [])
  | otherwise = (prev, findPost, next)
  where findPost = L.find (\p -> bid p == postId) posts
        post = fromJust findPost
        prev = filter (\p -> ordinal p < ordinal post) posts
        next = filter (\p -> ordinal p > ordinal post) posts
