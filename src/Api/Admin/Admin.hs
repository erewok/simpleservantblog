{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Admin.Admin where

import           Control.Monad.Except
import           Control.Monad.IO.Class                  (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Char8                   as B
import           Data.Monoid                             ((<>))
import           Data.Pool                               (Pool, withResource)
import qualified Data.Text                               as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types        (Query (..))
import           GHC.Generics
import           Servant
import           Servant.Elm
import           Servant.HTML.Blaze
import           Servant.Server.Experimental.Auth        (AuthHandler)
import           Servant.Server.Experimental.Auth.Cookie
import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A
import qualified Web.Users.Types                         as WU

import           Api.Admin.Login                         (Username)
import           Html.Home                               (pageSkeleton)
import           Models.Author                           (Author (..))
import qualified Models.Post                             as Post


data ResultResp = ResultResp {
  status        :: !T.Text
  , description :: !T.Text
} deriving (Eq, Show, Generic)

instance ElmType ResultResp
instance FromJSON ResultResp
instance ToJSON ResultResp

type AdminApi =
  "admin" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "user" :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Post '[JSON] Author
  :<|> "admin" :> "user" :> Capture "id" Int :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "user" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "post" :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Post '[JSON] Post.BlogPost
  :<|> "admin" :> "post" :> Capture "id" Int :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "post" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

adminHandlers :: Pool Connection -> Server AdminApi
adminHandlers conn = adminPage
                :<|> userAddH
                :<|> userUpdateH
                :<|> userDeleteH
                :<|> blogPostAddH
                :<|> blogPostUpdateH
                :<|> blogPostDeleteH
  where userAddH newUser _ = go $ addUser newUser
        userUpdateH userId user _ = go $ updateUser userId user
        userDeleteH userId _ = go $ deleteUser userId
        blogPostAddH newPost _ = go $ addPost newPost
        blogPostUpdateH postId post _ = go $ updatePost postId post
        blogPostDeleteH postId _ = go $ deletePost postId
        go = withResource conn

adminPage :: Username -> Handler Html
adminPage username = return $ docTypeHtml $ pageSkeleton $
      H.div ! A.class_ "row main" $
        H.div ! A.id "admin-page" $
          H.div ! A.class_ "admin-page-box" $
            H.div ! A.class_ "twelve columns" $ do
              H.p "Hello:"
              H.p $ H.text $ T.pack $ show username

addUser :: Author -> Connection -> Handler Author
addUser newUser conn = do
  let q = "insert into author values (?, ?, ?)"
  res <- liftIO $ query conn q newUser
  if null res then throwError err400 else return $ Prelude.head res

updateUser :: Int -> Author -> Connection -> Handler ResultResp
updateUser userId newUser conn = do
  let q = Query $ B.unwords ["update author set firstname = ?, lastname = ?, email = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (firstName newUser
                                   , lastName newUser
                                   , email newUser
                                   , userId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user updated"

deleteUser :: Int -> Connection -> Handler ResultResp
deleteUser userId conn = do
  let q = "delete from author where id = ?"
  result <- liftIO $ execute conn q (Only userId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user deleted"

addPost :: Post.BlogPost -> Connection -> Handler Post.BlogPost
addPost newPost conn = do
  result <- liftIO $ addPost' newPost conn
  case result of
    (x:_) -> return x
    []    -> throwError err404

addPost' :: Post.BlogPost -> Connection -> IO [Post.BlogPost]
addPost' newPost conn = do
  let q = "insert into post values (?, ?, ?)"
  query conn q newPost

updatePost :: Int -> Post.BlogPost -> Connection -> Handler ResultResp
updatePost postId newPost conn = do
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

deletePost :: Int -> Connection -> Handler ResultResp
deletePost postId conn = do
  let q = "delete from post where id = ?"
  result <- liftIO $ execute conn q (Only postId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "post deleted"
