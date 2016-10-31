{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User
    ( UserApi
    , userHandlers
    ) where


import           Control.Monad.IO.Class     (liftIO)
import           Data.Pool                  (withResource)
import           Data.Text

import           Database.PostgreSQL.Simple

import           Servant

import           Models.Author              (Author)

data SearchType = FirstName Text
                  | LastName Text

type UserApi = "user" :> Capture "firstName" Text  :> Get  '[JSON] [Author]
  :<|> "user" :> Capture "lastName" Text  :> Get  '[JSON] [Author]

userHandlers conn = userFNameSearchH
                   :<|> userLNameSearchH
  where userFNameSearchH name = withResource conn $ flip getUser (FirstName name)
        userLNameSearchH name = withResource conn $ flip getUser (LastName name)

getUser :: Connection -> SearchType -> Handler [Author]
getUser conn searchValue = case searchValue of
    FirstName fname -> do
      let q = "select from author where firstName = ?"
      liftIO $ query conn q (Only fname)
    LastName lname -> do
      let q = "select from author where lastName = ?"
      liftIO $ query conn q (Only lname)
    _ -> return []
