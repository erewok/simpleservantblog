{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Admin.MediaAdmin
  ( MediaAdminApi
  , mediaAdminHandlers
  ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class                  (liftIO)
import qualified Data.ByteString.Char8                   as B
import           Data.Int                                (Int64)
import           Data.Pool                               (Pool, withResource)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types        (Query (..))
import qualified Data.Text                               as T
import           Servant
import           Servant.Server.Experimental.Auth.Cookie
import           Servant.HTML.Blaze
import           Servant.Multipart
import           System.Directory                        (copyFile)

import           Api.Errors                              (appJson404)
import           Api.Login
import           Api.Types                               (ResultResp(..), AttachForm(..))
import qualified Models.Media                            as M
import           Types


type MediaAdminApi = "admin" :> "media" :> AuthProtect "cookie-auth" :> Get '[JSON] [M.Media]
  :<|> "admin" :> "media" :> Capture "id" Int  :> AuthProtect "cookie-auth" :> Get '[JSON] M.Media
  :<|> "admin" :> "media" :> MultipartForm Tmp (MultipartData Tmp) :> AuthProtect "cookie-auth" :> Post '[JSON] ResultResp
  :<|> "admin" :> "media" :> Capture "id" Int :> ReqBody '[JSON] M.Media :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "media" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "post" :> "media" :> ReqBody '[JSON] AttachForm  :> AuthProtect "cookie-auth" :> Post '[JSON] ResultResp
  :<|> "admin" :> "post" :> "media" :> Capture "mid" Int :> Capture "pid" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

mediaAdminHandlers :: ServerT MediaAdminApi SimpleHandler
mediaAdminHandlers = mediaGetAllH
                     :<|> mediaGetDetailH
                     :<|> mediaPostH
                     :<|> mediaPutH
                     :<|> mediaDeleteH
                     :<|> attachMediaToPostH
                     :<|> deleteMediaFromPostH

mediaGetAllH :: WithMetadata Username -> SimpleHandler [M.Media]
mediaGetAllH uname = runHandlerDbHelper $ \conn -> liftIO $ query_ conn "select * from media"

getMedia :: Int -> Connection -> IO (Maybe M.Media)
getMedia mediaId conn = do
  let q = "select * from media where id = ?"
  result <- liftIO $ query conn q (Only mediaId)
  case result of
    (x:_)-> return $ Just x
    _     -> return Nothing

mediaGetDetailH :: Int -> WithMetadata Username -> SimpleHandler M.Media
mediaGetDetailH mediaId uname = runHandlerDbHelper $ \conn -> do
  result <- liftIO $ getMedia mediaId conn
  case result of
    Nothing   -> throwError err404
    Just media -> return media

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

mediaPostH :: MultipartData Tmp -> WithMetadata Username -> SimpleHandler ResultResp
mediaPostH multipartData uname = runHandlerDbHelper $ \conn -> do
  fileLocs <- forM (files multipartData) $ \file -> do
    savedPath <- liftIO $ saveFileToStatic (fdPayload file) (fdFileName file)
    liftIO $ createMediaWithLoc (fdFileName file) savedPath conn
  return $ ResultResp "success" "media saved"

mediaPutH :: Int -> M.Media -> WithMetadata Username -> SimpleHandler ResultResp
mediaPutH mediaId editedMedia uname = runHandlerDbHelper $ \conn -> do
  let q = "update media set name = ? where id = ?"
  result <- liftIO $ execute conn q (M.name editedMedia
                                   , mediaId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "media updated"


mediaDeleteH :: Int -> WithMetadata Username -> SimpleHandler ResultResp
mediaDeleteH mediaId uname = runHandlerDbHelper $ \conn -> do
  -- must detach media from all posts where it's connected
  let deleteQ = "delete from postmedia where media_id = ?"
  result' <- liftIO $ execute conn deleteQ (Only mediaId)
  case result' of
    0 -> throwError err400
    _ -> do -- now we can delete the media
      let q = "delete from media where id = ?"
      result <- liftIO $ execute conn q (Only mediaId)
      case result of
        0 -> throwError err400
        _ -> return $ ResultResp "success" "user deleted"

attachMediaToPostH :: AttachForm -> WithMetadata Username -> SimpleHandler ResultResp
attachMediaToPostH (AttachForm postId mediaId) uname = runHandlerDbHelper $ \conn -> do
  let q = "insert into post_media (post_id, media_id) values (?, ?)"
  result <- liftIO $ execute conn q (postId, mediaId)
  pure $ ResultResp "success" "post Media created"

deleteMediaFromPostH :: Int -> Int -> WithMetadata Username -> SimpleHandler ResultResp
deleteMediaFromPostH mediaId postId uname = runHandlerDbHelper $ \conn -> do
  let q = "delete from postmedia where post_id = ? and media_id = ?"
  result <- liftIO $ execute conn q (postId, mediaId)
  case result of
    0 -> throwError err400
    _ -> return $ ResultResp "success" "media detached from post"
