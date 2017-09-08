{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


import           Data.List
import           Data.Proxy
import qualified Data.Text as T
import           Data.Text.IO    (writeFile)
import           GHC.TypeLits    (KnownSymbol)
import           Prelude         (IO, ($))
import           Servant.Elm
import           Servant.Foreign
import           Servant.Multipart

import           Api

instance (KnownSymbol sym, HasForeign lang ftype sublayout)
    => HasForeign lang ftype (AuthProtect sym :> sublayout) where
    type Foreign ftype (AuthProtect sym :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy =
      foreignFor lang ftype (Proxy :: Proxy sublayout)

instance (HasForeign lang ftype sublayout)
    => HasForeign lang ftype (MultipartForm sym :> sublayout) where
    type Foreign ftype (MultipartForm sym :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy =
      foreignFor lang ftype (Proxy :: Proxy sublayout)

main :: IO ()
main = do
  let code = T.intercalate "\n\n" $
        "module Blog.Api exposing (..)" :
        "import Date exposing (..)" :
        "import Exts.Date exposing (..)" :
        defElmImports :
        generateElmForAPI postApi
  writeFile "client/Blog/Api.elm" code
  let adminCode = T.intercalate "\n\n" $
        "module Admin.AdminApi exposing (..)" :
        "import Date exposing (..)" :
        "import Exts.Date exposing (..)" :
        defElmImports :
        generateElmForAPI adminProxyApi
  writeFile "client/Admin/AdminApi.elm" adminCode
