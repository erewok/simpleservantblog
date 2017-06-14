module Html.Projects where

import           Control.Monad.Except
import qualified Data.Text                   as T
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Html.Home                   (PageType (..), homeSkeleton)
import           Models.Project              (Project (..))



type ProjectsApi = "projects" :> Get '[HTML] Html

projectPage :: H.Html
projectPage = docTypeHtml $ homeSkeleton $ NoJS $
      H.div ! A.class_ "row" $
        H.div ! A.id "projects-page" $
          H.div ! A.class_ "projects-page-box" $ H.p


projectDetail :: H.Html
projectDetail = undefined
