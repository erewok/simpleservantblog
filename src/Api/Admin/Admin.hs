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

import           Api.Admin.Login                         (Username(..))
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

-- Separated the HTML so we can use servant-elm to generate for APIs only below
type AdminBackend =
  "admin" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> AdminApi


type AdminApi = "admin" :> "user" :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Post '[JSON] Author
  :<|> "admin" :> "user" :> Capture "id" Int :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "user" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "post" :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Post '[JSON] Post.BlogPost
  :<|> "admin" :> "post" :> Capture "id" Int :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "post" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

adminBackendHandlers :: Pool Connection -> Server AdminBackend
adminBackendHandlers conn = adminPage
                  :<|> adminHandlers conn

adminHandlers :: Pool Connection -> Server AdminApi
adminHandlers conn = userAddH
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
adminPage uname = return $ docTypeHtml $ adminSkeleton uname

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

adminSkeleton :: Username -> H.Html
adminSkeleton uname = do
         H.head $ do
           H.title "Ekadanta.co / erik aker"
           H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
           H.link ! A.href "//fonts.googleapis.com/css?family=Raleway:400,300,600" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "assets/css/styles.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "/assets/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "/assets/images/favicon.ico" ! A.rel "icon"
           H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""
           H.script ! A.type_ "text/javascript" ! A.src "assets/js/admin-elm.min.js" $ ""
         H.body $
           H.div ! A.class_ "container" $ do
            H.div ! A.id "elm-admin" ! A.class_ "admin-main" $ ""
            H.script ! A.type_ "text/javascript" $
                  H.text $ T.unlines [
                    "const node = document.getElementById('elm-admin'); "
                    , "const options = { 'username' :'" <> T.pack (username uname) <> "'}"
                    , "var app = Elm.Admin.embed(node, options); "
                    , "hljs.initHighlightingOnLoad();"
                    ]
