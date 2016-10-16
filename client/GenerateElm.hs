{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


import           Data.List
import           Data.Proxy
import           GHC.TypeLits    (KnownSymbol)
import           Servant.Elm
import           Servant.Foreign

import           Api

-- instance (KnownSymbol sym, HasForeign lang ftype sublayout)
--     => HasForeign lang ftype (AuthProtect sym :> sublayout) where
--     type Foreign ftype (AuthProtect sym :> sublayout) = Foreign ftype sublayout
--
--     foreignFor lang ftype Proxy req =
--       foreignFor lang ftype (Proxy :: Proxy sublayout) req

main :: IO ()
main = do
  let code = intercalate "\n\n" $
        "module Api exposing (..)" :
        "import Date exposing (..)" :
        "import Exts.Date exposing (..)" :
        defElmImports :
        generateElmForAPI blogApi
  writeFile "client/Api.elm" code
  -- let adminCode = intercalate "\n\n" $
  --       "module AdminApi exposing (..)" :
  --       "import Date exposing (..)" :
  --       "import Exts.Date exposing (..)" :
  --       defElmImports :
  --       generateElmForAPI adminProxyApi
  -- writeFile "client/AdminApi.elm" adminCode
