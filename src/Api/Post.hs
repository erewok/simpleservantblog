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

import           Models.Post                    (BlogPost, blogPostColumns)


type PostApi = "post" :> Get '[JSON] [BlogPost]
  :<|> "post" :> Capture "id" Int  :> Get  '[JSON] BlogPost
  :<|> "post" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost

postHandlers conn = blogPostListH
              :<|> blogPostDetailH
              :<|> blogPostAddH
  where blogPostListH = withResource conn listPosts
        blogPostDetailH postId = withResource conn $ flip getPost postId
        blogPostAddH newPost = withResource conn $ flip addPost newPost

listPosts :: Connection -> Handler [BlogPost]
listPosts conn = do
  let q = "select ? from post"
  liftIO $ query conn q (Only blogPostColumns)

getPost :: Connection -> Int -> Handler BlogPost
getPost conn postId = do
  let q = "select ? from post where id = ?"
  result <- liftIO $ query conn q (blogPostColumns, postId)
  case result of
    (x:_) -> return x
    []    -> throwError err404

addPost :: Connection -> BlogPost -> Handler BlogPost
addPost conn newPost = do
  let q = "insert into post values (?, ?, ?)"
  result <- liftIO $ query conn q newPost
  case result of
    (x:_) -> return x
    []    -> throwError err404
