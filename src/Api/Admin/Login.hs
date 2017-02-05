{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
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
import qualified Web.Users.Postgresql                    as WUP
import qualified Web.Users.Types                         as WU

import           Html.Home                               (pageSkeleton
                                                        , PageType(..)
                                                        , redirectPage)
import           Models.Author                           (Author (..))


newtype Username = Username { username :: String } deriving (Eq, Show, Generic)

instance Serialize Username

type instance AuthCookieData = Username

type LoginApi = "login" :> Get '[HTML] Html
                :<|> "login" :> ReqBody '[FormUrlEncoded] LoginForm
                             :> Post '[HTML] (Headers '[Header "set-cookie" B.ByteString] Html)


loginServer :: Pool Connection -> AuthCookieSettings -> RandomSource -> ServerKey -> Server LoginApi
loginServer conn settings rs key = loginPageH
            :<|> loginPostH
            where
              loginPageH = return $ loginPage True
              loginPostH loginF = withResource conn $ loginPost loginF settings rs key

loginPost :: LoginForm
            -> AuthCookieSettings
            -> RandomSource
            -> ServerKey
            -> Connection
            -> Handler (Headers '[Header "set-cookie" B.ByteString] Html)
loginPost loginF settings rs key conn = do
  let uname = lfUsername loginF
  authResult <- liftIO $ WU.authUser conn uname (WU.PasswordPlain $ lfPassword loginF) 12000000
  case authResult of
    Nothing -> return $ addHeader "" (loginPage False)
    Just sessionid -> do
      userid <- liftIO $ WU.getUserIdByName conn uname
      case userid of
        Nothing -> return $ addHeader "" (loginPage False)
        Just uid ->
          addSession
            settings -- the settings
            rs       -- random source
            key      -- server key
            (Username $ T.unpack uname)
            (redirectPage "/admin")

data LoginForm = LoginForm
 { lfUsername :: !T.Text
 , lfPassword :: !T.Text
 } deriving (Eq, Show)


instance FromFormUrlEncoded LoginForm where
 fromFormUrlEncoded d = do
   username <- case lookup "username" d of
     Nothing -> Left "username field is missing"
     Just  x -> return x
   password <- case lookup "password" d of
     Nothing -> Left "password field is missing"
     Just  x -> return x
   return LoginForm
     { lfUsername = username
     , lfPassword = password }

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
    H.input ! A.type_ "text" ! A.name "username" ! A.id "usernameField"
  H.div ! A.class_ "five columns" $ do
    H.label ! A.for "passwdField" $ "password"
    H.input ! A.type_ "password" ! A.name "password"  ! A.id "passwdField"
  H.input ! A.class_ "button-primary" ! A.type_ "submit" ! A.value "submit"
