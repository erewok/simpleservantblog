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


type UserAdminApi = "admin" :> "user" :> AuthProtect "cookie-auth" :> Get '[JSON] [Author]
  :<|> "admin" :> "user" :> Capture "id" Int  :> AuthProtect "cookie-auth" :> Get '[JSON] Author
  :<|> "admin" :> "user" :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Post '[JSON] Author
  :<|> "admin" :> "user" :> Capture "id" Int :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "user" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

userAdminHandlers :: Pool Connection -> Server UserAdminApi
userAdminHandlers conn = getUsersH
                         :<|> userDetailH
                         :<|> userAddH
                         :<|> userUpdateH
                         :<|> userDeleteH
    where getUsersH :: WithMetadata Username -> Handler [Author]
          getUsersH _ = withResource conn getUsers
          userDetailH userId _ = withResource conn $ getUserById userId
          userAddH newUser _ = withResource conn $ addUser newUser
          userUpdateH userId user _ = withResource conn $ updateUser userId user
          userDeleteH userId _ = withResource conn $ deleteUser userId


getUsers :: Connection -> Handler [Author]
getUsers conn = do
  let q = "select * from author"
  liftIO $ query_ conn q

getUserById :: Int -> Connection -> Handler Author
getUserById userId conn = do
  let q = "select * from author where id = ?"
  res <- liftIO $ query conn q (Only userId)
  case res of
    (x:_) -> return x
    _     -> throwError $ appJson404 "Unknown author"

addUser :: Author -> Connection -> Handler Author
addUser newAuthor conn = do
  let q = "insert into author (firstname, lastname) values (?, ?) returning id"
  res <- liftIO $ query conn q (firstName newAuthor
                              , lastName newAuthor) :: Handler [Only Int]
  case res of
    [] -> throwError err400
    (uid:_) -> do
      author <- liftIO $ query conn "select * from author where id = ?" uid
      if null author then throwError err400 else return $ Prelude.head author

updateUser :: Int -> Author -> Connection -> Handler ResultResp
updateUser userId author conn = do
  let q = Query $ B.unwords ["update author set firstname = ?, lastname = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (firstName author
                                   , lastName author
                                   , aid author)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user updated"

deleteUser :: Int -> Connection -> Handler ResultResp
deleteUser authorId conn = do
  let q = "delete from author where id = ?"
  result <- liftIO $ execute conn q (Only authorId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user deleted"
