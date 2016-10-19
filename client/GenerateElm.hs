{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


import           Data.List
import           Data.Proxy
import           GHC.TypeLits    (KnownSymbol)
import           Servant.Elm
import           Servant.Foreign

import           Api

instance (KnownSymbol sym, HasForeign lang ftype sublayout)
    => HasForeign lang ftype (AuthProtect sym :> sublayout) where
    type Foreign ftype (AuthProtect sym :> sublayout) = Foreign ftype sublayout

    foreignFor lang ftype Proxy =
      foreignFor lang ftype (Proxy :: Proxy sublayout)

main :: IO ()
main = do
  let code = intercalate "\n\n" $
        "module Blog.Api exposing (..)" :
        "import Date exposing (..)" :
        "import Exts.Date exposing (..)" :
        defElmImports :
        generateElmForAPI blogApi
  writeFile "client/Blog/Api.elm" code
  let adminCode = intercalate "\n\n" $
        "module Admin.AdminApi exposing (..)" :
        "import Date exposing (..)" :
        "import Exts.Date exposing (..)" :
        defElmImports :
        generateElmForAPI adminProxyApi
  writeFile "client/Admin/AdminApi.elm" adminCode
