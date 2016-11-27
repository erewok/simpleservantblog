module Admin exposing (..)

import Platform.Cmd exposing (none)
import Date exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (..)
import String exposing (..)
import Navigation
import RouteUrl

import Admin.AdminApi exposing (..)
import Admin.Routes exposing (..)
import Admin.Types exposing (..)
import Admin.Views exposing (..)
import Blog.Api as Api
import Blog.Types as BlogTypes


type alias InvokeOptions = { username : String }

main : Program InvokeOptions
main =
    RouteUrl.programWithFlags
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : InvokeOptions -> (Model, Cmd Msg)
init options =
  let
      state =
          { route = AdminMainR, user = options.username, content = Nothing}
  in
      ( state, none )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
          model ! []
        GoToAdminMain ->
          { model | content = Nothing, route = AdminMainR } ! []
        FromAdminBackend backend ->
          case backend of
            AdminResultResp _ ->
              { model | content = Nothing } ! []
            _ ->
              { model | content = Just backend } ! []
        FromAdminFrontend frontend ->
          case frontend of
            AdminGetList someList ->
              case someList of
                ListPosts ->
                  { model | route = AdminPostListR } ! [ retrieveList ListPosts ]
                ListUsers ->
                  { model | route = AdminUserListR } ! [ retrieveList ListUsers ]
                ListSeries ->
                  { model | route = AdminSeriesListR } ! [ retrieveList ListSeries ]
            AdminGetDetail someThing ->
              case someThing of
                DetailPost postId ->
                  { model | route = AdminPostDetailR postId } ! [ retrievePost postId ]
                DetailSeries seriesId ->
                    { model | route = AdminSeriesDetailR seriesId } ! [ retrieveSeries seriesId ]
                DetailUser userId ->
                  { model | route = AdminUserDetailR userId } ! [ retrieveUser userId ]
            AdminDelete item ->
              model ! [ deleteItem item ]
            AdminCreate item ->
              model ! [ createItem item ]
            AdminEdit item ->
              model ! [ editItem item ]
        Error error ->
          { model | content = Just <| BackendError error } ! []
