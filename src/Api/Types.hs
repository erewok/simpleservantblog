{-# LANGUAGE DeriveGeneric #-}

module Api.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import GHC.Generics
import  Servant.Elm

data ResultResp = ResultResp {
  status        :: !T.Text
  , description :: !T.Text
} deriving (Eq, Show, Generic)

instance ElmType ResultResp
instance FromJSON ResultResp
instance ToJSON ResultResp
