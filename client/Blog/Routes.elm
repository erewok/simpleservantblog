module Blog.Routes exposing (..)

import String
import UrlParser exposing (Parser, parsePath, (</>), map, int, oneOf, s, string)
import Navigation exposing (Location)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import Blog.Api exposing (..)
import Blog.Types exposing (..)
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import Debug exposing (..)


delta2url : Model -> Model -> Maybe UrlChange
delta2url previous current =
    case previous.route == current.route of
        True ->
            Nothing

        False ->
            Just <| UrlChange NewEntry <| toUrl current.route


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map PostDetailRoute (s "posts" </> int)
        , map SeriesPostDetailRoute (s "series" </> int </> s "posts" </> int)
        , map HomeRoute (s "")
        ]


fromUrl : Location -> Maybe Route
fromUrl location =
    parsePath routeParser location


location2messages : Location -> List Msg
location2messages location =
    case fromUrl location of
        Just route ->
            case route of
                HomeRoute ->
                    [ FromFrontend SeePostList ]

                PostDetailRoute postId ->
                    [ FromFrontend <| SeePostDetail postId ]

                SeriesPostDetailRoute seriesId postId ->
                    [ FromFrontend <| SeeSeriesPostDetail postId seriesId ]

        Nothing ->
            [ FromFrontend SeePostList ]


toUrl : Route -> String
toUrl route =
    case route of
        HomeRoute ->
            "#"

        PostDetailRoute postId ->
            "#posts/" ++ toString postId

        SeriesPostDetailRoute seriesId postId ->
            "#series/" ++ toString seriesId ++ "/posts/" ++ toString postId
