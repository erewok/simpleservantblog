module Admin exposing (..)

import Platform.Cmd exposing (none)
import Date exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (..)
import String exposing (..)
import Task exposing (Task, perform, succeed)
import Navigation
import RouteUrl

import Admin.AdminApi exposing (..)
import Admin.Routes exposing (..)
import Admin.Types exposing (..)


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
          { route = AdminMain, user = toString options.username,
           content = Nothing, error = Nothing }
  in
      ( state, none )

update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            model ! []
        GoToAdminMain ->
          model ! []
        ServerList _ ->
          model ! []
        ServerDetail _ _ ->
          model ! []
        SeeListContent _ ->
          model ! []
        SeeDetailContent _ ->
          model ! []
        Error error ->
          { model | error = Just error } ! []

view : Model -> Html Msg
view state =
  div [ class "container"] [
    header [ id "page-header", class "top-nav"] [
      div [ class "row" ] [
        div [ class "four columns", style [("margin-top", "2%")] ] [
          text "Users"
          ]
        , div [ class "four columns", style [("margin-top", "2%")] ] [
          text "Posts"
          ]
        , div [ class "four columns", style [("margin-top", "2%")] ] [
          text <| "Hi, ", (text state.user)
          ]
        ]
      ]
    ]
