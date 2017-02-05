{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}

module Models.Media (
  PostMedia(..)
  , postMediaQueryByPostId
  , createPostMediaTable
  ) where

import           Control.Applicative                ((<$>), (<*>))
import           Data.Aeson
import qualified Data.ByteString.Char8              as B
import           Data.Maybe
import qualified Data.Text                          as T
import           Data.Time                          (UTCTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (Query (..))
import           GHC.Generics
import           Prelude                            (Eq, Int, Show, ($), (.))
import           Servant.Elm


data PostMedia = PostMedia {
  pmid            :: !Int
  , postid        :: Maybe Int
  , pmname        :: !T.Text
  , pmurl         :: !T.Text
  , pmlocation    :: !T.Text
  , pmdescription :: Maybe T.Text
  } deriving (Eq, Show, Generic)
instance ElmType PostMedia
instance ToJSON PostMedia
instance FromRow PostMedia where
  fromRow = PostMedia <$> field <*> field <*> field
    <*> field <*> field <*> field

postMediaQueryByPostId :: Query
postMediaQueryByPostId = Query $ B.unwords [
                                  "select pm.id, pm.post_id, pm.name, pm.url, pm.location, pm.description "
                                  , "from post_media pm where pm.post_id = ?"
                                  ]

createPostMediaTable :: Query
createPostMediaTable =
  [sql|
     CREATE TABLE post_media (
       id          serial primary key,
       post_id     integer,
       name        text NOT NULL,
       url         text NOT NULL,
       location    text NOT NULL,
       description text NOT NULL,
       CONSTRAINT postid_series_fkey FOREIGN KEY (post_id)
           REFERENCES public.post (id) MATCH SIMPLE
           ON UPDATE NO ACTION ON DELETE NO ACTION
    );
  |]
