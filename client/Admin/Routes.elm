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
        [ format AdminPostList (s "posts")
        , format AdminPostDetail (s "posts"  </> int)
        , format AdminUserList (s "users")
        , format AdminUserDetail (s "users" </> int)
        , format AdminMain (s "")
        ]

fromUrl : Location -> Result String Route
fromUrl location =
    parse identity routeParser (String.dropLeft 1 location.hash)


location2messages : Location -> List Msg
location2messages location =
    case fromUrl location of
        Ok route ->
            case route of
                AdminMain ->
                    [ GoToAdminMain ]
                AdminPostList ->
                  [ SeeListContent <| ListPosts [] ]
                AdminPostDetail postId ->
                  [ SeeDetailContent <| DetailPost postId ]
                AdminUserList ->
                  [ SeeListContent <| ListUsers [] ]
                AdminUserDetail userId ->
                  [ SeeDetailContent <| DetailUser userId ]
        Err error ->
            [ Error error ]


toUrl : Route -> String
toUrl route =
    case route of
        AdminMain ->
            "#"
        AdminPostList ->
          "#posts"
        AdminPostDetail postId ->
          "#posts/" ++ toString postId
        AdminUserList ->
          "#users"
        AdminUserDetail userId ->
          "#users/" ++ toString userId
