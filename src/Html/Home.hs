{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.Home where

import qualified Data.Text                   as T
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Servant
import           Servant.HTML.Blaze

type HomePage = Get '[HTML] Html

data PageType a = NoJS a
              | HighlightElm a

homePage :: Handler Html
homePage = pure $ pageSkeleton $ HighlightElm elmApp

pageSkeleton :: PageType H.Html -> H.Html
pageSkeleton pageType@(NoJS content) = docTypeHtml $ do
  pageHead pageType
  H.body $
    H.div ! A.class_ "container" $ do
      topNav
      content
      pageFooter
pageSkeleton pageType@(HighlightElm a) = docTypeHtml $ do
  pageHead pageType
  H.body $
    H.div ! A.class_ "container" $ do
      topNav
      a
      pageFooter

pageHead :: PageType H.Html -> H.Html
pageHead pageType = H.head $ do
  H.title "Ekadanta.co / erik aker"
  H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
  H.link ! A.href "//fonts.googleapis.com/css?family=Raleway:400,300,600" ! A.rel "stylesheet" ! A.type_ "text/css"
  H.link ! A.href "assets/css/styles.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
  H.link ! A.href "/assets/images/favicon.ico" ! A.rel "icon"
  headScripts pageType

headScripts :: PageType H.Html -> H.Html
headScripts (NoJS _) = ""
headScripts (HighlightElm _) = do
  H.link ! A.href "/assets/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
  H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""
  H.script ! A.type_ "text/javascript" ! A.src "assets/js/elm.min.js" $ ""

topNav :: H.Html
topNav = H.header ! A.id "page-header" ! A.class_ "top-nav" $
    H.div ! A.class_ "row" $ do
      H.div ! A.class_ "four columns" ! A.style "margin-top: 2%" $
        H.a ! A.class_ "button" ! A.style "border: none;" ! href "/" $
          H.h5 "EKADANTA"
      H.div ! A.class_ "two columns" ! A.style "margin-top: 2%" $ ""
      H.div ! A.class_ "two columns" ! A.style "margin-top: 2%" $
        H.a ! A.class_ "button" ! A.style "border: none;" ! href "/about" $ "about"
      H.div ! A.class_ "two columns" ! A.style "margin-top: 2%" $
        H.a ! A.class_ "button" ! A.style "border: none;" ! href "https://twitter.com/erewok" $ "@erewok"
      H.div ! A.class_ "two columns" ! A.style "margin-top: 2%" $
        H.a ! A.class_ "button" ! A.style "border: none;"  ! href "https://github.com/pellagic-puffbomb" $
          H.img ! A.src "/assets/images/GitHub-Mark-32px.png"

pageFooter :: H.Html
pageFooter = H.footer ! A.id "page-footer" $
  H.div ! A.class_ "row" $ do
    H.p "Copyright (c) 2016 Erik Aker"
    H.script $ H.text ga

elmApp :: H.Html
elmApp = do
  H.div ! A.id "elm" ! A.class_ "main" $ ""
  H.script ! A.type_ "text/javascript" $
    "var node = document.getElementById('elm'); var app = Elm.Main.embed(node); hljs.initHighlightingOnLoad();"

ga :: T.Text
ga = T.unlines ["(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){"
              , "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),"
              , "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)"
              , "})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');"
              , "ga('create', 'UA-89190801-1', 'auto');"
              , "ga('send', 'pageview');"]
