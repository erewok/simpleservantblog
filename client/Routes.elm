module Routes exposing (..)

import String
import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string)
import Navigation exposing (Location)
import Json.Decode as Json
import Json.Decode.Extra exposing (lazy)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Api exposing (..)
import Types exposing (..)


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ format HomeRoute (s "")
        , format PostDetailRoute (s "posts" </> int)
        , format SeriesPostDetailRoute (s "series" </> int </> s "posts" </> int)
        ]


decode : Location -> Result String Route
decode location =
    parse identity matchers (String.dropLeft 1 location.pathname)


encode : Route -> String
encode route =
    case route of
        HomeRoute ->
            "/"

        PostDetailRoute i ->
            "/posts/" ++ toString i

        SeriesPostDetailRoute i j ->
            "/series/" ++ toString i ++ "/posts/" ++ toString j


navigate : Route -> Cmd msg
navigate route =
    Navigation.newUrl (encode route)


linkTo : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
linkTo route attrs content =
    a ((linkAttrs route) ++ attrs) content


linkAttrs : Route -> List (Attribute msg)
linkAttrs route =
    let
        path =
            encode route
    in
        [ href path
        , attribute "data-navigate" path
        ]


catchNavigationClicks : (String -> msg) -> Attribute msg
catchNavigationClicks tagger =
    onWithOptions "click"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map tagger (Json.at [ "target" ] pathDecoder))


pathDecoder : Json.Decoder String
pathDecoder =
    Json.oneOf
        [ Json.at [ "data-navigate" ] Json.string
        , Json.at [ "parentElement" ] (lazy (\_ -> pathDecoder))
        , Json.fail "no path found for click"
        ]
