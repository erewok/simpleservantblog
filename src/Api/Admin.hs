{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Admin where

import           Control.Monad.Except
import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Char8              as B
import           Data.Maybe
import           Data.Pool                  (withResource)
import           Data.Proxy
import qualified Data.Text as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types   (Query(..))
import GHC.Generics
import           Models.Author              (Author(..))
import           Models.Post                (BlogPost(..))
import           Servant
import           Servant.Elm


type AdminApi = "admin" :> "user" :> ReqBody '[JSON] Author :> Post '[JSON] Author
           :<|> "admin" :> "user" :> Capture "id" Int :> ReqBody '[JSON] Author :> Put '[JSON] ResultResp
           :<|> "admin" :> "user" :> Capture "id" Int :> Delete '[JSON] ResultResp
           :<|> "admin" :> "post" :> ReqBody '[JSON] BlogPost :> Post '[JSON] BlogPost
           :<|> "admin" :> "post" :> Capture "id" Int :> ReqBody '[JSON] BlogPost :> Put '[JSON] ResultResp
           :<|> "admin" :> "post" :> Capture "id" Int :> Delete '[JSON] ResultResp

adminHandlers conn = userAddH
                :<|> userUpdateH
                :<|> userDeleteH
                :<|> blogPostAddH
                :<|> blogPostUpdateH
                :<|> blogPostDeleteH
  where userAddH newUser = withResource conn $ flip addUser newUser
        userUpdateH userId user = withResource conn $ flip (flip updateUser userId) user
        userDeleteH userId = withResource conn $ flip deleteUser userId
        blogPostAddH newPost = withResource conn $ flip addPost newPost
        blogPostUpdateH postId post = withResource conn $ flip (flip updatePost postId) post
        blogPostDeleteH postId = withResource conn $ flip deletePost postId

addUser :: Connection -> Author -> Handler Author
addUser conn newUser = do
  let q = "insert into author values (?, ?, ?)"
  res <- liftIO $ query conn q newUser
  if null res then throwError err400 else return $ head res

updateUser :: Connection -> Int -> Author -> Handler ResultResp
updateUser conn userId newUser = do
  let q = Query $ B.unwords ["update author set firstname = ?, lastname = ?, email = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (firstName newUser
                                   , lastName newUser
                                   , email newUser
                                   , userId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user updated"

deleteUser :: Connection -> Int -> Handler ResultResp
deleteUser conn userId = do
  let q = "delete from author where id = ?"
  result <- liftIO $ execute conn q (Only userId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user deleted"

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

updatePost :: Connection -> Int -> BlogPost -> Handler ResultResp
updatePost conn postId newPost = do
  let q = Query $ B.unwords ["update post set authorid = ?, title = ?, body = ?, "
                           , "seriesid = ?, synopsis = ?, pubdate = ?, ordinal = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (authorId newPost
                                   , title newPost
                                   , body newPost
                                   , seriesId newPost                                   
                                   , synopsis newPost
                                   , pubdate newPost
                                   , ordinal newPost
                                   , postId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "blogpost updated"

deletePost :: Connection -> Int -> Handler ResultResp
deletePost conn postId = do
  let q = "delete from post where id = ?"
  result <- liftIO $ execute conn q (Only postId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "post deleted"

data ResultResp = ResultResp {
  status :: T.Text
  , description :: T.Text
} deriving (Eq, Show, Generic)

instance ElmType ResultResp
instance FromJSON ResultResp
instance ToJSON ResultResp
