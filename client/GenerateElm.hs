
import           Data.List
import           Servant.Elm

import           Api

main :: IO ()
main = do
  let code = intercalate "\n\n" $
        "module Api exposing (..)" :
        defElmImports :
        generateElmForAPI blogApi
  writeFile "client/Api.elm" code
