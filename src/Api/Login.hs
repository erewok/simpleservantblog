{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Login where


import           Control.Lens
import           Control.Monad.IO.Class                  (liftIO)
import           Control.Monad.Reader                    (ask)
import           Data.Pool                               (Pool, withResource)
import           Data.Serialize                          (Serialize)
import qualified Data.Text                               as T
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Servant
import           Servant.HTML.Blaze
import           Servant.Server.Experimental.Auth.Cookie
import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A
import           Web.FormUrlEncoded                      (FromForm)
import qualified Web.Users.Types                         as WU
import qualified Web.Users.Postgresql                    as WUP

import           Config
import           Html.Home                               (PageType (..),
                                                          pageSkeleton,
                                                          redirectPage)
import           Types


data Username = Username { username :: String, userid :: Int } deriving (Eq, Show, Generic)

instance Serialize Username

type instance AuthCookieData = Username


type Cookied a = (Headers '[Header "set-cookie" EncryptedSession] a)

type LoginApi = "login" :> Get '[HTML] Html
                :<|> "login" :> ReqBody '[FormUrlEncoded] LoginForm
                             :> Post '[HTML] (Headers '[Header "set-cookie" EncryptedSession] Html)


data LoginForm = LoginForm
 { lfUsername :: !T.Text
 , lfPassword :: !T.Text
 } deriving (Eq, Show, Generic)


instance FromForm LoginForm

loginHandlers :: ServerT LoginApi SimpleHandler
loginHandlers = loginPageH :<|> loginPostH

loginPageH :: SimpleHandler Html
loginPageH = pure $ loginPage True

loginPostH :: LoginForm -> SimpleHandler (Headers '[Header "set-cookie" EncryptedSession] Html)
loginPostH loginF = do
  appConfig <- ask
  let conn = appConfig ^. getPool
      security = appConfig ^. getConfig ^. getSecurity
      uname = lfUsername (loginF :: LoginForm)
      settings = security ^. cookieSettings
      rs = security ^. randomSource
      key = security ^. serverKey
  withResource conn $ \db -> do
    authResult <- liftIO $ WU.authUser db uname (WU.PasswordPlain $ lfPassword loginF) 12000000
    case authResult of
      Nothing -> return $ addHeader emptyEncryptedSession (loginPage False)
      Just _ -> do
        userid <- liftIO $ WU.getUserIdByName db uname
        case userid of
          Nothing -> return $ addHeader emptyEncryptedSession (loginPage False)
          Just uid ->
            addSession
              settings -- the settings
              rs       -- random source
              key      -- server key
              (Username (T.unpack uname) (fromIntegral uid))
              (redirectPage "/admin")

loginPage :: Bool -> H.Html
loginPage firstTime = docTypeHtml $ pageSkeleton $ NoJS $
      H.div ! A.class_ "row main" $
        H.div ! A.id "login-page" $
          H.div ! A.class_ "login-page-box" $ do
            H.div ! A.class_ "u-full-width" $
              H.h2 "Login"
            let formOrError = if firstTime then loginForm else H.p "Incorrect username/password"
            formOrError

loginForm :: H.Html
loginForm = H.form ! A.method "post" ! A.action "/login" $ do
  H.div ! A.class_ "five columns" $ do
    H.label ! A.for "usernameField" $ "username"
    H.input ! A.type_ "text" ! A.name "lfUsername" ! A.id "usernameField"
  H.div ! A.class_ "five columns" $ do
    H.label ! A.for "passwdField" $ "password"
    H.input ! A.type_ "password" ! A.name "lfPassword"  ! A.id "passwdField"
  H.input ! A.class_ "button-primary" ! A.type_ "submit" ! A.value "submit"
