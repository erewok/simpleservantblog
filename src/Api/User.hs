{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User
    ( UserApi
    , userHandlers
    ) where


import           Control.Monad.Except
import           Control.Monad.IO.Class             (liftIO)
import           Data.Maybe
import           Data.Pool                          (withResource)
import           Data.Proxy
import           Data.Text

import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (fromRow)

import           Servant

import           Network.Wai
import           Network.Wai.Handler.Warp           as Warp

import           Models.Author                      (Author)

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
  where userAddH newUser = withResource conn $ flip addUser newUser
        userFNameSearchH name = withResource conn $ flip getUser (FirstName name)
        userLNameSearchH name = withResource conn $ flip getUser (LastName name)

addUser :: Connection -> Author -> Handler [Author]
addUser conn newUser = do
  let q = "insert into author values (?, ?, ?)"
  liftIO $ query conn q newUser

getUser :: Connection -> SearchType -> Handler [Author]
getUser conn searchValue = case searchValue of
    FirstName fname -> do
      let q = "select from author where firstName = ?"
      liftIO $ query conn q (Only fname)
    LastName lname -> do
      let q = "select from author where lastName = ?"
      liftIO $ query conn q (Only lname)
    _ -> return []
