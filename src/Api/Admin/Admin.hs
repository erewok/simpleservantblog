{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Admin.Admin where

import           Control.Exception                       (catch)
import           Control.Monad.Except
import           Control.Monad.IO.Class                  (liftIO)
import           Data.Aeson
import qualified Data.ByteString.Char8                   as B
import           Data.Int                                (Int64)
import           Data.Monoid                             ((<>))
import           Data.Pool                               (Pool, withResource)
import qualified Data.Text                               as T
import           Data.Time                               (getCurrentTime)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types        (Query (..))
import           GHC.Generics
import           Servant
import           Servant.Elm
import           Servant.HTML.Blaze
import           Servant.Multipart
import           Servant.Server.Experimental.Auth.Cookie
import           System.Directory                        (copyFile)
import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A

import           Api.Admin.Login                         (Username (..))
import           Api.Errors
import           Models.Author                           (Author (..))
import qualified Models.Media                            as M
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

adminBackendHandlers :: Pool Connection -> Server AdminBackend
adminBackendHandlers conn = adminPage
                  :<|> adminHandlers conn

type AdminApi = "admin" :> "user" :> AuthProtect "cookie-auth" :> Get '[JSON] [Author]
  :<|> "admin" :> "user" :> Capture "id" Int  :> AuthProtect "cookie-auth" :> Get '[JSON] Author
  :<|> "admin" :> "user" :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Post '[JSON] Author
  :<|> "admin" :> "user" :> Capture "id" Int :> ReqBody '[JSON] Author :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "user" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "post" :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Post '[JSON] Post.BlogPost
  :<|> "admin" :> "post" :> Capture "id" Int :> ReqBody '[JSON] Post.BlogPost :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "post" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "post" :> AuthProtect "cookie-auth" :> Get '[JSON] [Post.BlogPost]
  :<|> "admin" :> "post" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Get '[JSON] Post.BlogPost
  :<|> "admin" :> "series" :> ReqBody '[JSON] Post.BlogSeries :> AuthProtect "cookie-auth" :> Post '[JSON] Post.BlogSeries
  :<|> "admin" :> "series" :> Capture "id" Int :> ReqBody '[JSON] Post.BlogSeries :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "series" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "media" :> AuthProtect "cookie-auth" :> Get '[JSON] [M.Media]
  :<|> "admin" :> "media" :> Capture "id" Int  :> AuthProtect "cookie-auth" :> Get '[JSON] M.Media
  :<|> "admin" :> "media" :> MultipartForm MultipartData :> AuthProtect "cookie-auth" :> Post '[JSON] ResultResp
  :<|> "admin" :> "media" :> Capture "id" Int :> ReqBody '[JSON] M.Media :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "media" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "post" :> "media" :> Capture "pid" Int  :> Capture "mid" Int  :> AuthProtect "cookie-auth" :> Post '[JSON] ResultResp
  :<|> "admin" :> "post" :> "media" :> Capture "pid" Int  :> Capture "mid" Int  :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

adminHandlers :: Pool Connection -> Server AdminApi
adminHandlers conn = getUsersH
                :<|> userDetailH
                :<|> userAddH
                :<|> userUpdateH
                :<|> userDeleteH
                :<|> blogPostAddH
                :<|> blogPostUpdateH
                :<|> blogPostDeleteH
                :<|> blogPostGetAllH
                :<|> blogPostGetByIdH
                :<|> blogSeriesAddH
                :<|> blogSeriesUpdateH
                :<|> blogSeriesDeleteH
                :<|> mediaGetAllH
                :<|> mediaGetDetailH
                :<|> mediaPostH
                :<|> mediaPutH
                :<|> mediaDeleteH
                :<|> attachMediaToPostH
                :<|> deleteMediaFromPostH
  where getUsersH _ = go getUsers
        userDetailH userId _ = go $ getUserById userId
        userAddH newUser _ = go $ addUser newUser
        userUpdateH userId user _ = go $ updateUser userId user
        userDeleteH userId _ = go $ deleteUser userId
        blogPostAddH newPost _ = go $ addPost newPost
        blogPostUpdateH postId post _ = go $ updatePost postId post
        blogPostDeleteH postId _ = go $ deletePost postId
        blogPostGetAllH _ = go getAllPosts
        blogPostGetByIdH postId _ = go $ getPostById postId
        blogSeriesAddH newSeries _ = go $ addSeries newSeries
        blogSeriesUpdateH seriesId series _ = go $ updateSeries seriesId series
        blogSeriesDeleteH seriesId _ = go $ deleteSeries seriesId
        mediaGetAllH _ = go getAllMedia
        mediaGetDetailH mediaId _ = go $ getMediaById mediaId
        mediaPostH newMedia _ = go $ createMedia newMedia
        mediaPutH mediaId editedMedia _ = go $ updateMedia mediaId editedMedia
        mediaDeleteH mediaId _ = go $ deleteMedia mediaId
        attachMediaToPostH postId mediaId _ = go $ attachPostMedia postId mediaId
        deleteMediaFromPostH postId mediaId _ = go $ detachPostMedia postId mediaId
        go = withResource conn

adminPage :: WithMetadata Username -> Handler Html
adminPage uname = return $ docTypeHtml $ adminSkeleton $ wmData uname

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

getAllPosts :: Connection -> Handler [Post.BlogPost]
getAllPosts conn = liftIO $ query_ conn "select * from post"

addPost :: Post.BlogPost -> Connection -> Handler Post.BlogPost
addPost newPost conn = do
  result <- liftIO $ addPost' newPost conn
  case result of
    []    -> throwError err404
    (x:_) -> do
      retrieveResult <- liftIO $ getPost (fst x) conn
      case retrieveResult of
        Just post -> return post
        Nothing   -> throwError err400

getPostById :: Int -> Connection -> Handler Post.BlogPost
getPostById postId conn = do
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


addSeries :: Post.BlogSeries -> Connection -> Handler Post.BlogSeries
addSeries newSeries conn = do
  let q = "insert into series (name, description, parentid) values (?, ?, ?) returning id"
  result <- liftIO $ query conn q (Post.name newSeries
                                  , Post.description newSeries
                                  , Post.parentid newSeries) :: Handler [Only Int]
  case result of
    []    -> throwError err404
    (sid:_) -> do
      series <- liftIO $ query conn "select * from series where id = ?" sid
      if null series then throwError err404 else return $ Prelude.head series

updateSeries :: Int -> Post.BlogSeries -> Connection -> Handler ResultResp
updateSeries seriesId newSeries conn = do
  let q = Query $ B.unwords ["update series set name = ?, description = ?, parentid = ? "
                           , "where id = ?"]
  result <- liftIO $ execute conn q (Post.name newSeries
                                   , Post.description newSeries
                                   , Post.parentid newSeries
                                   , seriesId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "series updated"

deleteSeries :: Int -> Connection -> Handler ResultResp
deleteSeries seriesId conn = do
  let q = "delete CASCADE from series where id = ?"
  result <- liftIO $ execute conn q (Only seriesId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "series and posts deleted"

getAllMedia :: Connection -> Handler [M.Media]
getAllMedia conn = liftIO $ query_ conn "select * from media"

getMedia :: Int -> Connection -> IO (Maybe M.Media)
getMedia mediaId conn = do
  let q = "select * from media where id = ?"
  result <- liftIO $ query conn q (Only mediaId)
  case result of
    (x:_)-> return $ Just x
    _     -> return Nothing

getMediaById :: Int -> Connection -> Handler M.Media
getMediaById mediaId conn = do
  result <- liftIO $ getMedia mediaId conn
  case result of
    Nothing   -> throwError err404
    Just post -> return post

saveFileToStatic :: FilePath -> T.Text -> IO FilePath
saveFileToStatic fileLocTemp filename = do
  let newFileName = "/opt/server/media/" ++ T.unpack filename
  copyFile fileLocTemp newFileName
  return newFileName

urlFromFileLoc :: FilePath -> FilePath
urlFromFileLoc path = T.unpack $ T.replace "/opt/server" "" $ T.pack path

createMediaWithLoc :: T.Text -> FilePath -> Connection -> IO Int64
createMediaWithLoc name path conn = do
  let q = "insert into media (name, location, url) values (?, ?, ?)"
  execute conn q (name, urlFromFileLoc path, path)

createMedia :: MultipartData -> Connection -> Handler ResultResp
createMedia multipartData conn = do
  fileLocs <- forM (files multipartData) $ \file -> do
    savedPath <- liftIO $ saveFileToStatic (fdFilePath file) (fdFileName file)
    liftIO $ createMediaWithLoc (fdFileName file) savedPath conn
  return $ ResultResp "success" "media saved"

updateMedia :: Int -> M.Media -> Connection -> Handler ResultResp
updateMedia mediaId editedMedia conn = do
  let q = "update media set name = ? where id = ?"
  result <- liftIO $ execute conn q (M.name editedMedia
                                   , mediaId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "media updated"


deleteMedia :: Int -> Connection -> Handler ResultResp
deleteMedia mediaId conn = do
  let q = "delete from media where id = ?"
  result <- liftIO $ execute conn q (Only mediaId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "user deleted"


attachPostMedia :: Int -> Int -> Connection -> Handler ResultResp
attachPostMedia postId mediaId conn = do
  let q = "insert into post_media (post_id, media_id) values (?, ?)"
  result <- liftIO $ execute conn q (postId, mediaId)
  pure $ ResultResp "success" "post Media created"

detachPostMedia ::Int -> Int -> Connection -> Handler ResultResp
detachPostMedia postId mediaId conn = do
  let q = "delete from postmedia where post_id = ? and media_id = ?"
  result <- liftIO $ execute conn q (postId, mediaId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "media detached from post"

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
