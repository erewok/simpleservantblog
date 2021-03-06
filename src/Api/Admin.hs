{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Admin where

import           Data.Monoid                             ((<>))
import           Data.Pool                               (Pool)
import qualified Data.Text                               as T
import           Database.PostgreSQL.Simple
import           Servant

import           Servant.HTML.Blaze

import           Servant.Server.Experimental.Auth.Cookie

import           Text.Blaze.Html5                        as H
import           Text.Blaze.Html5.Attributes             as A

import           Api.Login                         (Username (..))
import           Api.Admin.MediaAdmin
import           Api.Admin.PostAdmin
import           Api.Admin.SeriesAdmin
import           Api.Admin.UserAdmin
import           Types


-- Separated the HTML so we can use servant-elm to generate for APIs only below
type AdminBackend =
  "admin" :> AuthProtect "cookie-auth" :> Get '[HTML] Html
  :<|> AdminApi

type AdminApi = UserAdminApi :<|> PostAdminApi :<|> SeriesAdminApi :<|> MediaAdminApi

adminBackendHandlers :: ServerT AdminBackend SimpleHandler
adminBackendHandlers = adminPage :<|> adminHandlers

adminHandlers :: ServerT AdminApi SimpleHandler
adminHandlers = userAdminHandlers
                :<|> postAdminHandlers
                :<|> seriesAdminHandlers
                :<|> mediaAdminHandlers

adminPage :: WithMetadata Username -> SimpleHandler Html
adminPage uname = return $ docTypeHtml $ adminSkeleton $ wmData uname

adminSkeleton :: Username -> H.Html
adminSkeleton uname = do
         H.head $ do
           H.title "Ekadanta.co / erik aker"
           H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
           H.link ! A.href "//fonts.googleapis.com/css?family=Raleway:400,300,600" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "assets/css/styles.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "/assets/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
           H.link ! A.href "/assets/images/favicon.ico" ! A.rel "icon"
           H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""
           H.script ! A.type_ "text/javascript" ! A.src "assets/js/admin-elm.min.js" $ ""
         H.body $
           H.div ! A.class_ "container" $ do
            H.div ! A.id "elm-admin" ! A.class_ "admin-main" $ ""
            H.script ! A.type_ "text/javascript" $
                  H.text $ T.unlines [
                    "const node = document.getElementById('elm-admin'); "
                    , "const options = { 'username' :'" <> T.pack (username uname) <> "'}"
                    , "var app = Elm.Admin.embed(node, options); "
                    , "hljs.initHighlightingOnLoad();"
                    ]
