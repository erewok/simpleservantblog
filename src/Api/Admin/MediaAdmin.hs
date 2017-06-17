{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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
import           Servant.Multipart
import           System.Directory                        (copyFile)

import           Api.Errors                              (appJson404)
import           Api.Types  (ResultResp(..))
import qualified Models.Media                            as M
import qualified Models.Post                             as Post


type MediaAdminApi = "admin" :> "media" :> AuthProtect "cookie-auth" :> Get '[JSON] [M.Media]
  :<|> "admin" :> "media" :> Capture "id" Int  :> AuthProtect "cookie-auth" :> Get '[JSON] M.Media
  :<|> "admin" :> "media" :> MultipartForm MultipartData :> AuthProtect "cookie-auth" :> Post '[JSON] ResultResp
  :<|> "admin" :> "media" :> Capture "id" Int :> ReqBody '[JSON] M.Media :> AuthProtect "cookie-auth" :> Put '[JSON] ResultResp
  :<|> "admin" :> "media" :> Capture "id" Int :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp
  :<|> "admin" :> "post" :> "media" :> Capture "pid" Int  :> Capture "mid" Int  :> AuthProtect "cookie-auth" :> Post '[JSON] ResultResp
  :<|> "admin" :> "post" :> "media" :> Capture "pid" Int  :> Capture "mid" Int  :> AuthProtect "cookie-auth" :> Delete '[JSON] ResultResp

mediaAdminHandlers :: Pool Connection -> Server MediaAdminApi
mediaAdminHandlers conn = mediaGetAllH
                          :<|> mediaGetDetailH
                          :<|> mediaPostH
                          :<|> mediaPutH
                          :<|> mediaDeleteH
                          :<|> attachMediaToPostH
                          :<|> deleteMediaFromPostH
  where mediaGetAllH _ = go getAllMedia
        mediaGetDetailH mediaId _ = go $ getMediaById mediaId
        mediaPostH newMedia _ = go $ createMedia newMedia
        mediaPutH mediaId editedMedia _ = go $ updateMedia mediaId editedMedia
        mediaDeleteH mediaId _ = go $ deleteMedia mediaId
        attachMediaToPostH postId mediaId _ = go $ attachPostMedia postId mediaId
        deleteMediaFromPostH postId mediaId _ = go $ detachPostMedia postId mediaId
        go = withResource conn

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
