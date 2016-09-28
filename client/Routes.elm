module Routes exposing (..)

import String
import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string)
import Navigation exposing (Location)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Api exposing (..)
import Types exposing (..)
import RouteUrl exposing (HistoryEntry(..), UrlChange)


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    case current.route of
        HomeRoute ->
            Just <| UrlChange NewEntry "#/"

        PostDetailRoute i ->
            Just <| UrlChange NewEntry <| "#/posts/" ++ toString i

        SeriesPostDetailRoute i j ->
            Just <| UrlChange NewEntry <| "#/series/" ++ toString i ++ "/posts/" ++ toString j


location2messages : Location -> List Msg
location2messages location =
    case fromUrl location of
        Ok route ->
            case route of
                HomeRoute ->
                    [ FromFrontend SeePostList ]

                PostDetailRoute postId ->
                    [ FromFrontend <| SeePostDetail postId ]

                SeriesPostDetailRoute postId seriesId ->
                    [ FromFrontend <| SeeSeriesPostDetail postId seriesId ]

        Err error ->
            [ Error error ]


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ format HomeRoute (s "")
        , format PostDetailRoute (s "posts" </> int)
        , format SeriesPostDetailRoute (s "series" </> int </> s "posts" </> int)
        ]


fromUrl : Location -> Result String Route
fromUrl location =
    parse identity routeParser (String.dropLeft 1 location.pathname)
