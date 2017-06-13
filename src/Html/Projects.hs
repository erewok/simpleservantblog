module Html.Projects where

import           Control.Monad.Except
import qualified Data.Text                   as T
import           Text.Blaze.Html5            as H
import           Text.Blaze.Html5.Attributes as A

import           Servant
import           Servant.HTML.Blaze

import           Html.Home                   (PageType (..), homeSkeleton)

projectPage :: H.Html
projectPage = docTypeHtml $ homeSkeleton $ NoJS $
      H.div ! A.class_ "row" $
        H.div ! A.id "projects-page" $
          H.div ! A.class_ "projects-page-box" $ H.p
