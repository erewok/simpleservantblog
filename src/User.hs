{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module User
    ( UserApi
    , userHandlers
    ) where


import           Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Data.Maybe
import Data.Proxy
import Data.Text

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (fromRow)

import Servant

import           Network.Wai
import           Network.Wai.Handler.Warp as Warp

import Models (Author)

data SearchType = FirstName Text
                  | LastName Text
                  | BlogTitle Text
                  | RowId Int


type UserApi = "user" :> ReqBody '[JSON] Author :> Post '[JSON] [Author]
  :<|> "user" :> Capture "firstName" Text  :> Get  '[JSON] [Author]
  :<|> "user" :> Capture "lastName" Text  :> Get  '[JSON] [Author]

userHandlers conn = userAddH
                   :<|> userFNameSearchH
                   :<|> userLNameSearchH
  where userAddH = addUser conn
        userFNameSearchH name = getUser conn (FirstName name)
        userLNameSearchH name = getUser conn (LastName name)

addUser :: Connection -> Author -> Handler [Author]
addUser conn newUser = do
  let q = "insert into author values (?, ?, ?)"
  liftIO $ query conn q newUser

getUser :: Connection -> SearchType -> Handler [Author]
getUser conn searchValue = case searchValue of
    FirstName fname -> do
      let q = "select from author where first_name = ?"
      liftIO $ query conn q (Only fname)
    LastName lname -> do
      let q = "select from author where last_name = ?"
      liftIO $ query conn q (Only lname)
    _ -> return []
