module Admin.Routes exposing (..)

import String
import UrlParser exposing (Parser, parse, (</>), format, int, oneOf, s, string)
import Navigation exposing (Location)
import Html.Attributes exposing (href, attribute)
import Html exposing (Html, Attribute, a)
import Html.Events exposing (onWithOptions)
import RouteUrl exposing (HistoryEntry(..), UrlChange)
import Debug exposing (..)

import Admin.AdminApi exposing (..)
import Admin.Types exposing (..)


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
        [ format AdminPostListR (s "posts")
        , format AdminPostDetailR (s "posts"  </> int)
        , format AdminUserListR (s "users")
        , format AdminUserDetailR (s "users" </> int)
        , format AdminSeriesListR (s "series")
        , format AdminSeriesDetailR (s "series" </> int)
        , format AdminMainR (s "")
        ]

fromUrl : Location -> Result String Route
fromUrl location =
    parse identity routeParser (String.dropLeft 1 location.hash)


location2messages : Location -> List Msg
location2messages location =
    case fromUrl location of
        Ok route ->
            case route of
                AdminMainR ->
                    [ GoToAdminMain ]
                AdminPostListR ->
                  [ FromAdminFrontend <| AdminGetList ListPosts ]
                AdminPostDetailR postId ->
                  [ FromAdminFrontend <| AdminGetDetail <| DetailPost postId ]
                AdminUserListR ->
                  [ FromAdminFrontend <| AdminGetList ListUsers ]
                AdminUserDetailR userId ->
                  [ FromAdminFrontend <| AdminGetDetail <| DetailUser userId ]
                AdminSeriesListR ->
                  [ FromAdminFrontend <| AdminGetList ListSeries ]
                AdminSeriesDetailR seriesId ->
                  [ FromAdminFrontend <| AdminGetDetail <| DetailSeries seriesId ]
        Err error ->
            [ Error error ]


toUrl : Route -> String
toUrl route =
    case route of
        AdminMainR ->
            "#"
        AdminPostListR ->
          "#posts"
        AdminPostDetailR postId ->
          "#posts/" ++ toString postId
        AdminUserListR ->
          "#users"
        AdminUserDetailR userId ->
          "#users/" ++ toString userId
        AdminSeriesListR ->
          "#series"
        AdminSeriesDetailR seriesId ->
          "#series/" ++ toString seriesId
