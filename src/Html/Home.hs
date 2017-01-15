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
type BlogMain = Get '[HTML] Html
type ContactPage = Get '[HTML] Html

data PageType a = NoJS a
              | HighlightElm a

homePage :: Handler Html
homePage = pure $ pageSkeleton $ NoJS $ H.text "Coming Soon"

blogMain :: Handler Html
blogMain = pure $ pageSkeleton $ HighlightElm elmApp

contactPage :: Handler Html
contactPage = pure $ pageSkeleton $ NoJS $ H.text "Coming Soon"

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
  H.link ! A.href "/assets/images/favicon.ico" ! A.rel "icon"
  H.link ! A.href "/assets/css/styles.min.css" ! A.rel "stylesheet" ! A.type_ "text/css"
  headScripts pageType

headScripts :: PageType H.Html -> H.Html
headScripts (NoJS _) = ""
headScripts (HighlightElm _) = do
  H.link ! A.href "/assets/highlight/styles/default.css" ! A.rel "stylesheet" ! A.type_ "text/css"
  H.script ! A.src "/assets/highlight/highlight.pack.js" $ ""
  H.script ! A.type_ "text/javascript" ! A.src "assets/js/elm.min.js" $ ""

topNav :: H.Html
topNav = H.section ! A.class_ "nav" $ do
    H.nav ! A.class_ "small-nav twelve columns" $
      H.ul ! A.class_ "small-nav" $ navLinkList
    H.nav ! A.class_ "navigation" $
      H.div ! A.class_ "row" $ do
        H.div ! A.class_ "three columns" $
          H.a ! A.class_ "button" ! href "/" $
            H.h5 "EKADANTA"
        H.div ! A.class_ "nine columns" $
          H.ul ! A.class_ "main-navigation u-pull-right" $ navLinkList

navLinkList :: H.Html
navLinkList = do
  H.li $ H.a ! href "/posts" $ "Blog"
  H.li $ H.a ! href "/projects" $ "Projects"
  H.li $ H.a ! href "/about" $ "About"
  H.li $ H.a ! href "/contact" $ "Contact"

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
