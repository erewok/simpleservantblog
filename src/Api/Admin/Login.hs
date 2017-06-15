{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Admin.Login where


import           Control.Monad.IO.Class                  (liftIO)
import qualified Data.ByteString.Char8                   as B
import           Data.Pool                               (Pool, withResource)
import           Data.Serialize                          (Serialize)
import qualified Data.Text                               as T
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.Types        (Query (..))
import           GHC.Generics
import           Servant
import           Servant.Elm
import           Servant.HTML.Blaze
import           Servant.Server.Experimental.Auth        (AuthHandler)
import           Servant.Server.Experimental.Auth.Cookie
import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A
import           Web.FormUrlEncoded                      (FromForm)
import qualified Web.Users.Postgresql                    as WUP
import qualified Web.Users.Types                         as WU

import           Html.Home                               (PageType (..),
                                                          pageSkeleton,
                                                          redirectPage)
import           Models.Author                           (Author (..))


newtype Username = Username { username :: String } deriving (Eq, Show, Generic)

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

loginServer :: Pool Connection -> AuthCookieSettings -> RandomSource -> PersistentServerKey -> Server LoginApi
loginServer conn settings rs key = loginPageH
            :<|> loginPostH
            where
              loginPageH = return $ loginPage True
              loginPostH loginF = withResource conn $ loginPost loginF settings rs key

loginPost :: LoginForm
            -> AuthCookieSettings
            -> RandomSource
            -> PersistentServerKey
            -> Connection
            -> Handler (Headers '[Header "set-cookie" EncryptedSession] Html)
loginPost loginF settings rs key conn = do
  let uname = lfUsername (loginF :: LoginForm)
  authResult <- liftIO $ WU.authUser conn uname (WU.PasswordPlain $ lfPassword loginF) 12000000
  case authResult of
    Nothing -> return $ addHeader emptyEncryptedSession (loginPage False)
    Just sessionid -> do
      userid <- liftIO $ WU.getUserIdByName conn uname
      case userid of
        Nothing -> return $ addHeader emptyEncryptedSession (loginPage False)
        Just uid ->
          addSession
            settings -- the settings
            rs       -- random source
            key      -- server key
            (Username $ T.unpack uname)
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
