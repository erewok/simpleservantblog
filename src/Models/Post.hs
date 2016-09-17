{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Post (
  BlogSeries(..)
  , blogSeriesColumns
  , BlogPost
  , blogPostColumns
  , BlogSeriesWithPosts(..)
  ) where

import Prelude (Int, Eq, Show, Bool, (.), ($), (++))
import Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.Maybe
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.FromRow (field, FromRow, fromRow)
import           Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import           Data.Time (UTCTime)
import           GHC.Generics
import qualified Data.Text as T
import           Servant.Elm


blogSeriesColumns = "id, name, description, parent"
data BlogSeries = BlogSeries {
  sid :: !Int
  , name :: !T.Text
  , description :: !T.Text
  , parentid :: Maybe Int
} deriving (Eq, Show, Generic)

instance ElmType BlogSeries
instance FromJSON BlogSeries
instance ToJSON BlogSeries
instance FromRow BlogSeries where
  fromRow = BlogSeries <$> field <*> field <*> field <*> field

instance ToRow BlogSeries where
  toRow s =  [toField $ sid s
             , toField $ name s
             , toField $ description s
             , toField $ parentid s]

data BlogSeriesWithPosts = BlogSeriesWithPosts {
  bsid :: !Int
  , sname :: !T.Text
  , sdescription :: !T.Text
  , parent :: Maybe Int
  , posts :: [BlogPost]
  } deriving (Eq, Show, Generic)

instance FromJSON BlogSeriesWithPosts
instance ToJSON BlogSeriesWithPosts
instance ElmType BlogSeriesWithPosts


blogPostColumns = "id, authorid, seriesid, title, body, synopsis, " ++
                  "created, modified, pubdate, ordinal"
data BlogPost = BlogPost {
  bid :: !Int
  , authorId :: !Int
  , seriesId :: Maybe Int
  , title :: !T.Text
  , body :: Maybe T.Text
  , synopsis:: Maybe T.Text
  , created :: !UTCTime
  , modified :: Maybe UTCTime
  , pubdate :: Maybe UTCTime
  , ordinal :: Maybe Int
  } deriving (Eq, Show, Generic)

instance ElmType BlogPost
instance FromJSON BlogPost
instance ToJSON BlogPost
instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field

instance ToRow BlogPost where
  toRow p =  [toField $ bid p
             , toField $ authorId p
             , toField $ seriesId p
             , toField $ title p
             , toField $ body p
             , toField $ synopsis p
             , toField $ created p
             , toField $ modified p
             , toField $ pubdate p
             , toField $ ordinal p
             ]
