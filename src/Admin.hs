{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module Admin where

import           Control.Monad.IO.Class                  (liftIO)
import           Data.Monoid                             ((<>))
import           Data.Pool                               (Pool, withResource)
import           Database.PostgreSQL.Simple
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.Experimental.Auth.Cookie
import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A

import           Admin.Views
import           Api.Login                               (Username (..))
import qualified Models.Author                           as Author
import qualified Models.Post                             as Post


type AdminHtml = "admin" :> "edit" :> "posts" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "edit" :> "posts" :> Capture "postid" Int :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "edit" :> "series" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "edit" :> "authors" :> AuthProtect "cookie-auth" :> Get '[HTML] Html

adminHtmlHandlers :: Pool Connection -> Server AdminHtml
adminHtmlHandlers conn = editAllPostsH :<|> editOnePostH
                         :<|> editAllSeriesH :<|> editAllAuthorsH
  where editAllPostsH uname = go $ editAllPosts uname
        editOnePostH postId uname = go $ editOnePost uname postId
        editAllSeriesH uname = go $ editAllSeries uname
        editAllAuthorsH uname = go $ editAllAuthors uname
        go = withResource conn

editAllPosts :: WithMetadata Username -> Connection -> Handler Html
editAllPosts uname conn = do
  posts <- liftIO $ query_ conn "select * from post" :: Handler [Post.BlogPost]
  pure $ adminSkeleton (wmData uname) (adminEditTable "posts" posts)

editOnePost ::  WithMetadata Username -> Int -> Connection -> Handler Html
editOnePost uname postId conn = do
  result <- liftIO $ getPost postId conn
  case result of
    Nothing -> throwError err404
    Just post -> pure $ adminSkeleton (wmData uname) (adminEditSingle "posts" post)

getPost :: Int -> Connection -> IO (Maybe Post.BlogPost)
getPost postId conn = do
  let q = "select * from post where id = ?"
  result <- liftIO $ query conn q (Only postId)
  case result of
    (x:_)-> return $ Just x
    _     -> return Nothing

editAllSeries :: WithMetadata Username -> Connection -> Handler Html
editAllSeries uname conn = do
  series <- liftIO $ query_ conn "select * from series" :: Handler [Post.BlogSeries]
  pure $ adminSkeleton (wmData uname) (adminEditTable "series" series)

editAllAuthors :: WithMetadata Username -> Connection -> Handler Html
editAllAuthors uname conn = do
  authors <- liftIO $ query_ conn "select * from author" :: Handler [Author.Author]
  pure $ adminSkeleton (wmData uname) (adminEditTable "authors" authors)


adminEditTable :: (AdminRender a) => Text -> [a] -> Html
adminEditTable editType models = H.div ! A.class_ "admin-edit-table" $ do
  addNew editType
  toTableView models

adminEditSingle :: (AdminRender a) => Text -> a -> Html
adminEditSingle editType model = H.div ! A.class_ "edit-main" $
  H.div ! A.class_ "row" $
    toEditSingleView model


addNew :: Text -> Html
addNew addType = H.div ! A.class_ "row" $
  H.a ! A.href (H.toValue addType) $
    H.div ! A.class_ "two columns" $
      H.span ! A.class_ "add-new" $ H.text "+"

adminSkeleton :: Username -> H.Html -> H.Html
adminSkeleton uname content' = do
         H.head $ do
           H.title "Ekadanta.co / erik aker"
           H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
           H.link ! A.href "//fonts.googleapis.com/css?family=Raleway:400,300,600" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "/assets/css/styles.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "/assets/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "/assets/images/favicon.ico" ! A.rel "icon"
           H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""
           H.script "hljs.initHighlightingOnLoad();"
         H.body $ H.div ! A.class_ "container" $ do
           H.header ! A.id "page-header" ! A.class_ "top-nav" $
             H.div ! A.class_ "row" $ do
               H.div ! A.class_ "two columns" $
                 H.a ! A.class_ "button admin-button" $ H.text $ "Hi, " <> T.pack (username uname)
               H.div ! A.class_ "two columns" $
                 H.a ! A.class_ "button admin-button" $ "Users"
               H.div ! A.class_ "two columns" $
                 H.a ! A.class_ "button admin-button" $ "Series"
               H.div ! A.class_ "two columns" $
                 H.a ! A.class_ "button admin-button" $ "Posts"
               H.div ! A.class_ "two columns" $
                 H.a ! A.class_ "button admin-button" $ "Visit Site"
               H.div ! A.class_ "two columns" $
                 H.a ! A.class_ "button admin-button" $ "Settings"
           content'
