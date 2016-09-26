{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Models.Post (
  BlogSeries(..)
  , BlogPost(..)
  , PostSeries(..)
  , PostOverview(..)
  , postOverviewAllQuery
  , seriesPostsQuery
  ) where

import           Control.Applicative                ((<$>), (<*>))
import           Data.Aeson
import qualified Data.ByteString.Char8              as B
import           Data.Maybe
import qualified Data.Text                          as T
import           Data.Time                          (UTCTime)
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (Query(..))
import           GHC.Generics
import           Prelude                            (Bool, Eq, Int, Show, ($),
                                                     (++), (.))
import           Servant.Elm

--
-- Api Helpers for Frontend --
--

-- PostOverview
data PostOverview = PostOverview {
  pid                 :: !Int
  , ptitle             :: !T.Text
  , psynopsis          :: Maybe T.Text
  , ppubdate           :: Maybe UTCTime
  , pordinal           :: Maybe Int
  , pseriesid          :: Maybe Int
  , pseriesname        :: Maybe T.Text
  , pseriesdescription :: Maybe T.Text
  } deriving (Eq, Show, Generic)
instance ElmType PostOverview
instance ToJSON PostOverview
instance FromRow PostOverview where
  fromRow = PostOverview <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field

postOverviewAllQuery :: Query
postOverviewAllQuery = Query $ B.unwords [
                                  "select p.id, p.title, p.synopsis, p.pubdate, p.ordinal, "
                                  , "s.id, s.name, s.description from post p left join series s "
                                  , "on p.seriesid = s.id where p.pubdate is NOT NULL"
                                  ]

seriesPostsQuery :: Query
seriesPostsQuery = Query $ B.unwords [
                              "select id, authorid, seriesid, title, body, synopsis, "
                              , "created, modified, pubdate, ordinal "
                              , "from post where seriesid = (select seriesid from post p where p.id = ?) "
                              ,  "and pubdate is NOT NULL "
                              , "order by ordinal"
                              ]
data PostSeries = PostSeries {
  previous :: [BlogPost]
  , current :: BlogPost
  , next :: [BlogPost]
  , series :: BlogSeries
} deriving (Eq, Show, Generic)
instance ElmType PostSeries
instance ToJSON PostSeries


--
-- Table Definitions --
--
-- Table Definition Series
data BlogSeries = BlogSeries {
  sid           :: !Int
  , name        :: !T.Text
  , description :: !T.Text
  , parentid    :: Maybe Int
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

-- Post
data BlogPost = BlogPost {
  bid        :: !Int
  , authorId :: !Int
  , seriesId :: Maybe Int
  , title    :: !T.Text
  , body     :: Maybe T.Text
  , synopsis:: Maybe T.Text
  , created  :: !UTCTime
  , modified :: Maybe UTCTime
  , pubdate  :: Maybe UTCTime
  , ordinal  :: Maybe Int
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
