{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Models.Author (
  Author(..)
  , createAuthorTable
  ) where

import           Control.Applicative                ((<$>), (<*>))
import           Data.Aeson
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (Query (..))
import           GHC.Generics
import           Prelude                            (Eq, Int, Show, ($), (.))
import           Servant.Elm


data Author = Author {
  aid         :: !Int
  , userid    :: !Int
  , firstName :: !T.Text
  , lastName  :: !T.Text
  } deriving (Eq, Show, Generic)

instance ElmType Author
instance FromJSON Author
instance ToJSON Author
instance FromRow Author where
  fromRow = Author <$> field <*> field <*> field <*> field


instance ToRow Author where
  toRow p =  [ toField $ aid p
             , toField $ userid p
             , fieldA firstName
             , fieldA lastName]
    where
      fieldA = toField . ($ p)

createAuthorTable :: Query
createAuthorTable =
  [sql|
      CREATE TABLE author (
        id         serial primary key,
        userid     integer NOT NULL,
        firstname  character varying(40) NOT NULL,
        lastName   character varying(50),
        CONSTRAINT one_to_one_author_userid (userid),
        CONSTRAINT user_id_author_fk FOREIGN KEY (userid)
            REFERENCES public.login (lid) MATCH SIMPLE
            ON UPDATE NO ACTION ON DELETE CASCADE
      );
  |]
