{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Admin.PostAdmin
  ( PostAdminApi
  , postAdminHandlers
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
import           Api.Types  (ResultResp(..))
import qualified Models.Post                             as Post
import           Types

type PostAdminApi =  "admin" :> "post" :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Post '[JSON] Post.BlogPost
                     :<|> "admin" :> "post" :> Capture "id" Int :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
                    :<|> "admin" :> "post" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
                    :<|> "admin" :> "post" :> AuthProtect "cookie-auth" :> Get '[JSON] [Post.BlogPost]
                    :<|> "admin" :> "post" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Get '[JSON] Post.BlogPost


postAdminHandlers :: ServerT PostAdminApi SimpleHandler
postAdminHandlers = blogPostAddH
                    :<|> blogPostUpdateH
                    :<|> blogPostDeleteH
                    :<|> blogPostGetAllH
                    :<|> blogPostGetByIdH

blogPostGetAllH :: WithMetadata Username -> SimpleHandler [Post.BlogPost]
blogPostGetAllH uname = runHandlerDbHelper $ \conn -> liftIO $ query_ conn "select * from post"

blogPostAddH :: Post.BlogPost -> WithMetadata Username ->  SimpleHandler Post.BlogPost
blogPostAddH newPost uname = runHandlerDbHelper $ \conn -> do
  result <- liftIO $ addPost' newPost conn
  case result of
    []    -> throwError err404
    (x:_) -> do
      retrieveResult <- liftIO $ getPost (fst x) conn
      case retrieveResult of
        Just post -> return post
        Nothing   -> throwError err400

blogPostGetByIdH :: Int -> WithMetadata Username -> SimpleHandler Post.BlogPost
blogPostGetByIdH postId uname = runHandlerDbHelper $ \conn -> do
  result <- liftIO $ getPost postId conn
  case result of
    Nothing   -> throwError err404
    Just post -> return post

getPost :: Int -> Connection -> IO (Maybe Post.BlogPost)
getPost postId conn = do
  let q = "select * from post where id = ?"
  result <- liftIO $ query conn q (Only postId)
  case result of
    (x:_)-> return $ Just x
    _     -> return Nothing

addPost' :: Post.BlogPost -> Connection -> IO [(Int, T.Text)]
addPost' newPost conn = do
  let q = Query $ B.unwords ["insert into post (authorid, title, seriesid, "
                           , "synopsis, pubdate, body, ordinal, created, modified) "
                           , "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) returning id, title"]
  modified <- case Post.modified newPost of
    Nothing -> Just <$> getCurrentTime
    Just tm -> Just <$> pure tm
  created <- getCurrentTime
  liftIO $ query conn q (Post.authorId newPost
                        , Post.title newPost
                        , Post.seriesId newPost
                        , Post.synopsis newPost
                        , Post.pubdate newPost
                        , Post.body newPost
                        , Post.ordinal newPost
                        , created
                        , modified) :: IO [(Int, T.Text)]

blogPostUpdateH :: Int -> Post.BlogPost -> WithMetadata Username -> SimpleHandler ResultResp
blogPostUpdateH postId newPost uname = runHandlerDbHelper $ \conn -> do
  let q = Query $ B.unwords ["update post set authorid = ?, title = ?, body = ?, "
                           , "seriesid = ?, synopsis = ?, pubdate = ?, ordinal = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (Post.authorId newPost
                                   , Post.title newPost
                                   , Post.body newPost
                                   , Post.seriesId newPost
                                   , Post.synopsis newPost
                                   , Post.pubdate newPost
                                   , Post.ordinal newPost
                                   , postId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "blogpost updated"

blogPostDeleteH :: Int -> WithMetadata Username -> SimpleHandler ResultResp
blogPostDeleteH postId uname = runHandlerDbHelper $ \conn -> do
  let q = "delete from post where id = ?"
  result <- liftIO $ execute conn q (Only postId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "post deleted"
