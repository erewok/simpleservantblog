{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}

module Models.Projects (
  Project(..)
  , createProjectsTable
  ) where

import           Control.Applicative                ((<$>), (<*>))
import           Data.Aeson                         hiding (Series)
import           Data.Maybe
import qualified Data.Text                          as T
import           Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import           Database.PostgreSQL.Simple.SqlQQ
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (ToRow, toRow)
import           Database.PostgreSQL.Simple.Types   (Query (..))
import           GHC.Generics
import           Prelude                            (Eq, Int, Show, ($), (.))
import           Servant.Elm


data Project = Project {
  pid                  :: !Int
  , title              :: !T.Text
  , slug               :: !T.Text
  , description        :: Maybe T.Text
  , source             :: Maybe T.Text
  , source_url         :: Maybe T.Text
  , projectLocalUrl  :: Maybe T.Text
  , language           :: Maybe T.Text
  , list_order         :: Maybe Int
  } deriving (Eq, Show, Generic)

instance ElmType Project
instance FromJSON Project
instance ToJSON Project
instance FromRow Project where
  fromRow = Project <$> field <*> field <*> field
    <*> field <*> field <*> field
    <*> field <*> field <*> field

instance ToRow Project where
  toRow p =  [toField $ pid p
             , toField $ title p
             , toField $ slug p
             , toField $ description p
             , toField $ source p
             , toField $ source_url p
             , toField $ projectLocalUrl p
             , toField $ language p
             , toField $ list_order p
             ]


createProjectsTable :: Query
createProjectsTable =
   [sql|
         CREATE TABLE IF NOT EXISTS project (
            id            SERIAL UNIQUE,
            title         character varying(255) NOT NULL,
            slug          character varying(255) NOT NULL,
            description   text,
            source        text,
            source_url    text,
            project_local_url text,
            language      text,
            list_order    integer
         );
   |]
