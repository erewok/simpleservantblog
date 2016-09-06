{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Post (
  BlogPost
  ) where

import Prelude (Int, Eq, Show, Bool, (.), ($))
import Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.Maybe
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.FromRow (field, FromRow, fromRow)
import           Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics
import qualified Data.Text as T
import           Servant.Elm


type BlogPostId = Int
data BlogPost = BlogPost {
  bid :: !BlogPostId
  , authorId :: !Int
  , title :: !T.Text
  , body :: Maybe T.Text
  , published :: Bool
  , created :: !UTCTime
  , modified :: Maybe UTCTime
  , pubdate :: Maybe UTCTime
  , synopsis:: Maybe T.Text
  } deriving (Eq, Show, Generic)

instance ElmType BlogPost
instance FromJSON BlogPost
instance ToJSON BlogPost
instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field

instance ToRow BlogPost where
  toRow p =  [toField $ bid p
             ,fieldA title
             , fieldA (fromJust . synopsis)
             , toField $ published p
             , fieldA (fromJust . body)
             , toField $ authorId p
             , toField $ created p
             , toField $ (fromJust . modified) p
             , toField $ (fromJust . pubdate) p]
    where
      fieldA = toField . ($ p)
