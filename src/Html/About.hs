{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.About where

import           Data.Text                   (unlines)
import           Prelude                     (pure, ($))
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Servant
import           Servant.HTML.Blaze

import           Html.Home                   (pageSkeleton, PageType(..))


type AboutPage = Get '[HTML] Html

aboutPage :: Handler Html
aboutPage = pure $ docTypeHtml $ pageSkeleton $ NoJS aboutContent

aboutContent :: H.Html
aboutContent = H.div ! A.class_ "row main" $
  H.div ! A.id "about-page" $
    H.div ! A.class_ "about-page-box" $ do
      H.div ! A.class_ "six columns left-about" $
        H.div ! A.class_ "about-page-photo" $
          H.div ! A.class_ "about-page-photo-overlay" $ ""
      H.div ! A.class_ "six columns right-about" $
        H.div ! A.class_ "about-page-content" $ do
          H.h4 "Erik Aker"
          H.p $ H.text aboutText1
          H.p "His interests include the following: "
          H.ul $ do
            H.li "Functional Programming"
            H.li "Natural Language Processing"
            H.li "Surfing"
            H.li "Playing with Milo"

aboutText1 = unlines ["Formerly an English teacher and freelance writer, Erik Aker is a ",
                      "web application developer in San Diego."]
