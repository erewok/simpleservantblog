{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Models.Media (
  Media(..)
  , PostMedia(..)
  , postMediaQueryByPostId
  , createMediaTable
  , createPostMediaTable
  ) where

import           Control.Applicative                ((<$>), (<*>))
import           Data.Aeson
import qualified Data.ByteString.Char8              as B
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (Query (..))
import           GHC.Generics
import           Prelude                            (Bool, Eq, Int, Show, ($),
                                                     (.))
import           Servant.Elm


postMediaQueryByPostId :: Query
postMediaQueryByPostId = Query $ B.unwords [
                                  "select m.id, m.name, m.url, m.location, m.description, pm.is_featured "
                                , "from media m where "
                                , " inner join post_media pm on pm.media_id = m.id "
                                , "where pm.post_id = ?"
                                ]

data Media = Media {
  mediaid       :: !Int
  , name        :: !T.Text
  , url         :: !T.Text
  , location    :: !T.Text
  , description :: !T.Text
  } deriving (Eq, Show, Generic)
instance ElmType Media
instance ToJSON Media
instance FromJSON Media
instance FromRow Media where
  fromRow = Media <$> field <*> field <*> field <*> field <*> field

instance ToRow Media where
  toRow m =  [toField $ mediaid (m :: Media)
             , toField $ name m
             , toField $ url m
             , toField $ location m
             , toField $ description m
            ]

data PostMedia = PostMedia {
  postid       :: !Int
  , mediaid    :: !Int
  , isFeatured :: !Bool
  } deriving (Eq, Show, Generic)
instance ElmType PostMedia
instance ToJSON PostMedia
instance FromRow PostMedia where
  fromRow = PostMedia <$> field <*> field <*> field

instance ToRow PostMedia where
  toRow pm =  [toField $ postid (pm :: PostMedia)
             , toField $ mediaid (pm :: PostMedia)
             , toField $ isFeatured pm
            ]

createMediaTable :: Query
createMediaTable =
  [sql|
     CREATE TABLE media (
       id          serial primary key,
       name        text NOT NULL,
       url         text NOT NULL,
       location    text NOT NULL,
       description text NOT NULL
    );
  |]

createPostMediaTable :: Query
createPostMediaTable =
  [sql|
     CREATE TABLE post_media (
       post_id     integer NOT NULL,
       media_id    integer NOT NULL,
       is_featured bool,
       PRIMARY KEY(post_id, media_id),
       CONSTRAINT post_fkey FOREIGN KEY (post_id)
           REFERENCES public.post (id) MATCH SIMPLE
           ON UPDATE NO ACTION ON DELETE CASCADE,
       CONSTRAINT media_fkey FOREIGN KEY (media_id)
           REFERENCES public.media (id) MATCH SIMPLE
           ON UPDATE NO ACTION ON DELETE CASCADE
    );
  |]
