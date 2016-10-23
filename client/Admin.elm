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
        FromAdminBackend backend ->
          { model | content = Just backend } ! []
        FromAdminFrontend frontend ->
          case frontend of
            AdminGetList someList ->
              model ! [ retrieveSomeList someList ]
            AdminGetDetail someThing ->
              model ! [ retrieveThing someThing]
            AdminDelete item ->
              model ! [ deleteItem item ]
            AdminCreate item ->
              model ! [ createItem item ]
            AdminEdit item ->
              model ! [ editItem item ]
        Error error ->
          { model | content = Just <| BackendError error } ! []

retrieveSomeList : ListThing -> Cmd Msg
retrieveSomeList someList = case someList of
  ListPosts -> retrievePostList
  ListUsers -> retrieveUserList

retrieveThing : DetailThing -> Cmd Msg
retrieveThing someThing =  case someThing of
  DetailPost postId -> retrievePost postId
  DetailSeries seriesId -> retrieveSeries seriesId
  DetailUser userId -> retrieveUser userId

deleteItem : Item -> Cmd Msg
deleteItem item = case item of
  BlogPost -> deleteBlogPost item
  Author -> deleteAuthor item
  Series -> deleteSeries item

createItem : Item -> Cmd Msg
createItem item = case item of
  BlogPost -> createBlogPost item
  Author -> createAuthor item
  Series -> createSeries item

editItem : Item -> Cmd Msg
editItem item = case item of
  BlogPost -> editBlogPost item
  Author -> editAuthor item
  Series -> editSeries item


view : Model -> Html Msg
view state =
  div [ class "container"] [
    header [ id "page-header", class "top-nav"] [
      div [ class "row" ] [
        div [ class "four columns", style [("margin-top", "2%")] ] [
          a [ onClick (FromAdminFrontend <| AdminGetList ListUsers), class "button"]
            [ text "Users" ]
          ]
        , div [ class "four columns", style [("margin-top", "2%")] ] [
            a [ onClick (FromAdminFrontend <| AdminGetList ListPosts), class "button" ]
              [ text "Posts" ]
          ]
        , div [ class "four columns", style [("margin-top", "2%")] ] [
            a [ onClick GoToAdminMain, class "button" ]
              [ text <| "Hi, ", (text state.user) ]
          ]
        ]
      ]
    ]


retrievePostList : Cmd Msg
retrievePostList =
  Api.getPost
    |> Task.mapError toString
    |> Task.perform Error (\posts -> FromAdminBackend (AdminPostList posts))

retrieveUserList : Cmd Msg
retrieveUserList =
  getAdminUser
    |> Task.mapError toString
    |> Task.perform Error (\posts -> FromAdminBackend (AdminUserList posts))

retrievePost : BlogTypes.BlogPostId -> Cmd Msg
retrievePost postId =
    Api.getPostById postId
        |> Task.mapError toString
        |> Task.perform Error (\post -> FromAdminBackend (AdminPostDetail post))

retrieveSeries : SeriesId -> Cmd Msg
retrieveSeries seriesId =
  Api.getSeriesById seriesId
    |> Task.mapError toString
    |> Task.perform Error (\series -> FromAdminBackend (AdminSeriesDetail series))

retrieveUser : UserId -> Cmd Msg
retrieveUser userId =
  Api.getUserById userId
    |> Task.mapError toString
    |> Task.perform Error (\user -> FromAdminBackend (AdminUserDetail user))

deleteBlogPost : Api.BlogPost -> Cmd Msg
deleteBlogPost post =
  deleteAdminPostById post.bid
    |> Task.mapError toString
    |> Task.perform Error (\rr -> AdminResultResp rr)

deleteAuthor : Api.Author -> Cmd Msg
deleteAuthor author =
  deleteAdminUserById author.aid
    |> Task.mapError toString
    |> Task.perform Error (\rr -> AdminResultResp rr)

deleteSeries : Api.BlogSeries -> Cmd Msg
deleteSeries series =
  deleteAdminSeriesById series.sid
    |> Task.mapError toString
    |> Task.perform Error (\rr -> AdminResultResp rr)
