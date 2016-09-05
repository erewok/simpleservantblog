{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Blog (
  Author
  , BlogPost
  ) where

import           Data.Aeson
import           Data.Maybe
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.FromRow (field, FromRow, fromRow)
import           Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics
import qualified Data.Text as T
import           Servant.Elm


data Author = Author {
  id :: !Int
  , firstName :: !T.Text
  , lastName :: !T.Text
  , email :: !T.Text
  } deriving (Eq, Show, Generic)

instance ElmType Author
instance FromJSON Author
instance ToJSON Author
instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field <*> field


instance ToRow Author where
  toRow p =  [fieldA firstName
             , fieldA lastName
             , fieldA email]
    where
      fieldA = toField . ($ p)

data BlogPost = BlogPost {
  id :: !Int
  , title :: !T.Text
  , synopsis:: Maybe T.Text
  , published :: Bool
  , body :: !T.Text
  , authorId :: !Int
  , created :: !UTCTime
  , modified :: !UTCTime
  , pubdate :: !UTCTime
  } deriving (Eq, Show, Generic)

instance ElmType BlogPost
instance FromJSON BlogPost
instance ToJSON BlogPost
instance FromRow BlogPost where
  fromRow = BlogPost <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow BlogPost where
  toRow p =  [fieldA title
             , fieldA (fromJust . synopsis)
             , toField $ published p
             , fieldA body
             , toField $ authorId p
             , toField $ created p
             , toField $ modified p
             , toField $ pubdate p]
    where
      fieldA = toField . ($ p)
