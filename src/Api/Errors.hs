{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Errors where

import           Data.Aeson           (ToJSON, encode)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as LB
import           Data.Text            (Text)
import           GHC.Generics
import           Network.HTTP.Types   (HeaderName, hContentType)
import           Servant


jsonHeader :: (HeaderName, B.ByteString)
jsonHeader = (hContentType, "application/json")

data ErrorResp = ErrorResp {status  :: Text
                          , message :: Text
                          } deriving (Generic)
instance ToJSON ErrorResp

missingError :: Text -> LB.ByteString
missingError msg = encode ErrorResp { status = "missing", message = msg}

appJson404 :: Text -> ServantErr
appJson404 msg = err404  { errBody = missingError msg, errHeaders = [jsonHeader]}
