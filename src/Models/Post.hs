{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Post (
  BlogSeries
  , blogSeriesColumns
  , BlogPost
  , blogPostColumns
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
             , toField $ name s
             , toField $ (fromJust . parentid) s]

blogPostColumns = "id, authorid, seriesid, title, body, " ++
                  "published, created, modified, synopsis, pubdate"
data BlogPost = BlogPost {
  bid :: !Int
  , authorId :: !Int
  , seriesId :: Maybe Int
  , title :: !T.Text
  , body :: Maybe T.Text
  , published :: Bool
  , created :: !UTCTime
  , modified :: Maybe UTCTime
  , synopsis:: Maybe T.Text
  , pubdate :: Maybe UTCTime
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
             , toField $ (fromJust . seriesId) p
             , fieldA title
             , fieldA (fromJust . body)
             , toField $ published p
             , toField $ created p
             , toField $ (fromJust . modified) p
             , fieldA (fromJust . synopsis)
             , toField $ (fromJust . pubdate) p
             ]
    where
      fieldA = toField . ($ p)
