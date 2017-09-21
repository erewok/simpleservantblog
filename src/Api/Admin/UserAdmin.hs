{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Admin.UserAdmin
  ( UserAdminApi
  , userAdminHandlers
  ) where


import           Control.Monad.IO.Class                  (liftIO)
import qualified Data.ByteString.Char8                   as B
import           Data.Pool                               (Pool, withResource)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types        (Query (..))
import           Servant
import           Servant.Server.Experimental.Auth.Cookie

import           Api.Errors                              (appJson404)
import           Api.Login
import           Api.Types  (ResultResp(..))
import           Models.Author                           (Author (..))
import           Types


type UserAdminApi = "admin" :> "user" :> AuthProtect "cookie-auth" :> Get '[JSON] [Author]
  :<|> "admin" :> "user" :> Capture "id" Int  :> AuthProtect "cookie-auth" :> Get '[JSON] Author
  :<|> "admin" :> "user" :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Post '[JSON] Author
  :<|> "admin" :> "user" :> Capture "id" Int :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "user" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

userAdminHandlers :: ServerT UserAdminApi SimpleHandler
userAdminHandlers = getUsersH
                    :<|> userDetailH
                    :<|> userAddH
                    :<|> userUpdateH
                    :<|> userDeleteH

getUsersH :: WithMetadata Username -> SimpleHandler [Author]
getUsersH uname = runHandlerDbHelper $ \conn -> do
  let q = "select * from author"
  liftIO $ query_ conn q

userDetailH :: Int -> WithMetadata Username ->  SimpleHandler Author
userDetailH userId uname =  runHandlerDbHelper $ \conn -> do
  let q = "select * from author where id = ?"
  res <- liftIO $ query conn q (Only userId)
  case res of
    (x:_) -> return x
    _     -> throwError $ appJson404 "Unknown author"

userAddH :: Author -> WithMetadata Username -> SimpleHandler Author
userAddH newAuthor uname = runHandlerDbHelper $ \conn -> do
  let q = "insert into author (firstname, lastname) values (?, ?) returning id"
  res <- liftIO $ query conn q (firstName newAuthor
                              , lastName newAuthor) :: SimpleHandler [Only Int]
  case res of
    [] -> throwError err400
    (uid:_) -> do
      author <- liftIO $ query conn "select * from author where id = ?" uid
      if null author then throwError err400 else return $ Prelude.head author

userUpdateH :: Int -> Author -> WithMetadata Username -> SimpleHandler ResultResp
userUpdateH userId author uname = runHandlerDbHelper $ \conn -> do
  let q = Query $ B.unwords ["update author set firstname = ?, lastname = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (firstName author
                                   , lastName author
                                   , aid author)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user updated"

userDeleteH :: Int -> WithMetadata Username -> SimpleHandler ResultResp
userDeleteH authorId uname = runHandlerDbHelper $ \conn -> do
  let q = "delete from author where id = ?"
  result <- liftIO $ execute conn q (Only authorId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user deleted"
