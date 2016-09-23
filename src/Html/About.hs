{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Html.About where

import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Servant
import           Servant.HTML.Blaze

import Html.Home (pageSkeleton)


type AboutPage = Get '[HTML] Html

aboutPage :: Handler Html
aboutPage = return $ docTypeHtml $ pageSkeleton $
      H.div ! A.class_ "row main" $ do
        H.img ! A.src "/assets/images/apairatbeachsm.jpg" ! A.class_ "u-pull-left"
        H.p "Some Content"
        H.p ""
        H.p ""
