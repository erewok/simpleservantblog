{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Html.Home where

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Servant
import           Servant.HTML.Blaze


type HomePage = Get '[HTML] Html

homePage :: Handler Html
homePage = return $ docTypeHtml $ do
  H.head $ do
    H.title "Simple Servant Blog"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.link ! A.href "//fonts.googleapis.com/css?family=Raleway:400,300,600" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "assets/css/normalize.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "assets/css/skeleton.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "assets/css/styles.css" ! A.rel "stylesheet" ! A.type_ "text/css"
    H.link ! A.href "assets/images/favicon.ico" ! A.rel "icon"
    H.link ! A.href "/assets/highlight/styles/github.css" ! A.rel "stylesheet"
    H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""
    H.script ! A.type_ "text/javascript" ! A.src "assets/js/elm.js" $ ""
  H.body $
    H.div ! A.class_ "container" $ do
      H.div ! A.class_ "row" $ do
        H.div ! A.class_ "three columns" ! A.style "margin-top: 2%" $
          H.a ! A.class_ "button" ! A.style "border: none;" ! href "#" $
            H.h5 ! A.class_ "u-pull-right" $ "EKADANTA"
        H.div ! A.class_ "nine columns" ! A.style "margin-top: 2%" $
          H.div ! A.class_ "top-nav" $ do
            H.div ! A.class_ "two columns" $
              H.a ! A.class_ "button" ! A.style "border: none;" ! href "#" $ "blog"
            H.div ! A.class_ "one column" $ ""
            H.div ! A.class_ "two columns" $
              H.a ! A.class_ "button" ! A.style "border: none;" ! href "about" $ "about"
            H.div ! A.class_ "one column" $ ""
            H.div ! A.class_ "two columns" $
              H.a ! A.class_ "button" ! A.style "border: none;"  ! href "projects" $ "projects"
      H.div ! A.id "elm" $ ""
      H.script ! A.type_ "text/javascript" $
        "var node = document.getElementById('elm'); var app = Elm.Main.embed(node);"
