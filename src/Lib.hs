{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( BlogApi
    , blogApi
    , server
    , app
    ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class             (liftIO)
import           Data.Maybe
import Data.Pool (Pool)
import           Data.Proxy
import           Data.Text

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (fromRow)

import           Servant

import           Network.Wai
import           Network.Wai.Handler.Warp           as Warp

import           Models
import           Post
import           User

data SearchType = FirstName Text
                  | LastName Text
                  | BlogTitle Text
                  | RowId Int

type BlogApi = PostApi
              :<|> UserApi

apihandlers conn = postHandlers conn
                  :<|> userHandlers conn

blogApi :: Proxy BlogApi
blogApi = Proxy

server :: Pool Connection -> Server BlogApi
server = apihandlers

app :: Pool Connection -> Application
app conn = serve blogApi $ server conn
