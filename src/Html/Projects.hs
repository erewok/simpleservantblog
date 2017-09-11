{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Html.Projects where

import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Html.Home                   (PageType (..), homeSkeleton)
import           Models.Projects             (Project (..))



type ProjectsApi = "projects" :> Get '[HTML] Html

projectsServer :: Server ProjectsApi
projectsServer = projectPageH
  where projectPageH = return projectPage

projectPage :: H.Html
projectPage = docTypeHtml $ homeSkeleton $ NoJS $
      H.div ! A.class_ "row" $
        H.div ! A.id "projects-page" $
          H.div ! A.class_ "projects-page-box" $ H.p "Content Coming Soon"


projectDetail :: H.Html
projectDetail = undefined
