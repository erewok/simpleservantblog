{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Html.Home where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import           Servant
import Servant.HTML.Blaze


type HomePage = "home" :> Get '[HTML] Html

homePage :: Handler Html
homePage = return $ docTypeHtml $ do
  H.head $ do
    H.title "Simple Servant Blog"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.link ! A.href "//fonts.googleapis.com/css?family=Raleway:400,300,600" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "css/normalize.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "css/skeleton.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "images/favicon.png" ! A.rel "icon"
  H.body $ do
    H.div ! A.class_ "container" $ do
      H.div ! A.class_ "row" $ do
        H.div ! A.class_ "one-half column" ! A.style "margin-top: 25%" $ do
          H.h4 "Basic Page"
          H.p "This index.html page is a placeholder with the CSS, font and favicon. It's just waiting for you to add some content!"
