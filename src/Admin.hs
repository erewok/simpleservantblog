{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}


module Admin where

import           Control.Monad.IO.Class                  (liftIO)
import qualified Data.ByteString.Char8                   as B
import           Data.Monoid                             ((<>))
import           Data.Pool                               (Pool, withResource)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types        (Query (..))
import           Data.Time                               (getCurrentTime)
import           Data.Text                               (Text)
import qualified Data.Text                               as T
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.Experimental.Auth.Cookie
import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A

import           Admin.Forms
import           Admin.Views
import           Api.Login                               (Username (..))
import           Models                                  (ModelType(..))
import qualified Models.Author                           as Author
import qualified Models.Post                             as Post
import           Types



type AdminHtml = "admin" :> "edit" :> "posts" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "edit" :> "posts" :> "new" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "edit" :> "posts" :> Capture "postid" Int :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "edit" :> "posts" :> "save"
    :> ReqBody '[FormUrlEncoded] PostForm :> AuthProtect "cookie-auth" :> Post '[HTML] Html
  :<|> "admin" :> "edit" :> "series" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> "admin" :> "edit" :> "authors" :> AuthProtect "cookie-auth" :> Get '[HTML] Html

adminHtmlHandlers :: ServerT AdminHtml SimpleHandler
adminHtmlHandlers = editAllPostsH :<|> editNewPostH :<|> editOnePostH :<|> savePostH
                    :<|> editAllSeriesH :<|> editAllAuthorsH
  where editNewPostH uname = editOnePost uname Nothing
        editOnePostH postId uname = editOnePost uname (Just postId)

-- Post Editing
editAllPostsH :: WithMetadata Username -> SimpleHandler Html
editAllPostsH uname = runHandlerDbHelper $ \conn -> do
  posts <- liftIO $ query_ conn "select * from post" :: SimpleHandler [Post.BlogPost]
  pure $ adminSkeleton (wmData uname) (adminEditTable Posts posts)

editOnePost ::  WithMetadata Username -> Maybe Int -> SimpleHandler Html
editOnePost uname Nothing = runHandlerDbHelper $ \conn -> do  -- Create a brand-new post
  created <- liftIO getCurrentTime
  let lookupAuthor = "select * from author where userid = ?"
  authorLookupResult <- liftIO $ query conn lookupAuthor $ Only (userid $ wmData uname)
  authId <- case authorLookupResult of
    (auth:_) -> pure $ Author.aid auth
    _ -> throwError err404
  let bid = 0
      authorId = authId
      seriesId = Nothing
      title = "New post yeah!"
      body = Nothing
      synopsis = Nothing
      modified = Nothing
      pubdate = Nothing
      ordinal = Nothing
  let newPost = Post.BlogPost {..}
  pure $ adminSkeleton (wmData uname) (adminEditSingle Posts newPost)
editOnePost uname (Just postId) = runHandlerDbHelper $ \conn -> do -- lookup an existing post
  result <- liftIO $ getPost postId conn
  case result of
    Nothing -> throwError err404
    Just post -> pure $ adminSkeleton (wmData uname) (adminEditSingle Posts post)

getPost :: Int -> Connection -> IO (Maybe Post.BlogPost)
getPost postId conn = do
  let q = "select * from post where id = ?"
  result <- liftIO $ query conn q (Only postId)
  case result of
    (x:_)-> return $ Just x
    _     -> return Nothing

savePostH :: PostForm -> WithMetadata Username -> SimpleHandler Html
savePostH post uname = undefined

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

updatePost :: Int -> Post.BlogPost -> Connection -> IO Int
updatePost postId post conn = do
  let q = Query $ B.unwords ["update post set authorid = ?, title = ?, body = ?, "
                           , "seriesid = ?, synopsis = ?, pubdate = ?, ordinal = ? "
                           , "where id = ?"]
  result <- execute conn q (Post.authorId post
                           , Post.title post
                           , Post.body post
                           , Post.seriesId post
                           , Post.synopsis post
                           , Post.pubdate post
                           , Post.ordinal post
                           , postId)
  pure $ fromIntegral result

-- Series Editing
editAllSeriesH :: WithMetadata Username -> SimpleHandler Html
editAllSeriesH uname = runHandlerDbHelper $ \conn -> do
  series <- liftIO $ query_ conn "select * from series" :: SimpleHandler [Post.BlogSeries]
  pure $ adminSkeleton (wmData uname) (adminEditTable Series series)


-- Author Editing
editAllAuthorsH :: WithMetadata Username -> SimpleHandler Html
editAllAuthorsH uname = runHandlerDbHelper $ \conn -> do
  authors <- liftIO $ query_ conn "select * from author" :: SimpleHandler [Author.Author]
  pure $ adminSkeleton (wmData uname) (adminEditTable Authors authors)


-- Primary Markup determinants
adminEditTable :: (AdminRender a) => ModelType -> [a] -> Html
adminEditTable editType models = H.div ! A.class_ "admin-edit-table" $ do
  addNew $ makeNewItemLink editType
  toTableView models

adminEditSingle :: (AdminRender a) => ModelType -> a -> Html
adminEditSingle editType model = H.div ! A.class_ "edit-main" $
  H.div ! A.class_ "row" $
    H.form ! A.action (H.toValue $ saveLink editType) ! A.method "post" $ do
      toEditSingleView model
      H.input ! A.type_ "submit" ! A.class_ "button button-primary" ! A.value "save"

addNew :: Text -> Html
addNew link' = H.div ! A.class_ "row" $
  H.a ! A.href (H.toValue link' ) $
    H.div ! A.class_ "two columns" $
      H.span ! A.class_ "add-new" $ H.text "+"

-- helper functions
saveLink :: ModelType -> Text
saveLink Posts = "/admin/edit/posts/save"
saveLink Series = "/admin/edit/series/save"
saveLink Authors = "/admin/edit/authors/save"
saveLink Projects = "/admin/edit/projects/save"
saveLink Media = "/admin/edit/media/save"

makeNewItemLink :: ModelType -> Text
makeNewItemLink Posts = "/admin/edit/posts/new"
makeNewItemLink Series = "/admin/edit/series/new"
makeNewItemLink Authors = "/admin/edit/authors/new"
makeNewItemLink Projects = "/admin/edit/projects/new"
makeNewItemLink Media = "/admin/edit/media/new"


-- Page layout
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
