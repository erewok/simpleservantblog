{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models.Author (
  Author(..)
  , authorColumns
  ) where

import Prelude (Int, Eq, Show, (.), ($))
import Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.FromRow (field, FromRow, fromRow)
import           Database.PostgreSQL.Simple.ToRow (ToRow, toRow)
import           GHC.Generics
import qualified Data.Text as T
import           Servant.Elm


authorColumns = "id, firstname, lastname, email"
data Author = Author {
  aid :: !Int
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
  toRow p =  [ toField $ aid p
             , fieldA firstName
             , fieldA lastName
             , fieldA email]
    where
      fieldA = toField . ($ p)
