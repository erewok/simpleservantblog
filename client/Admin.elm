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
import Task exposing (Task, perform, succeed)
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
          {model | content = Nothing } ! []
        FromAdminBackend backend ->
          case backend of
            AdminResultResp _ ->
              { model | content = Nothing } ! []
            _ ->
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
  ListSeries -> retrieveSeriesList

retrieveThing : DetailThing -> Cmd Msg
retrieveThing someThing =  case someThing of
  DetailPost postId -> retrievePost postId
  DetailSeries seriesId -> retrieveSeries seriesId
  DetailUser userId -> retrieveUser userId

deleteItem : Item -> Cmd Msg
deleteItem item = case item of
  PI post -> deleteBlogPost post
  AI author -> deleteAuthor author
  SI series -> deleteSeries series

createItem : Item -> Cmd Msg
createItem item = case item of
  PI post -> createBlogPost post
  AI author -> createAuthor author
  SI series -> createSeries series

editItem : Item -> Cmd Msg
editItem item = case item of
  PI post -> editBlogPost post
  AI author -> editAuthor author
  SI series -> editSeries series

retrievePostList : Cmd Msg
retrievePostList =
  Api.getPost
    |> Task.mapError toString
    |> Task.perform Error (\posts -> FromAdminBackend <| AdminPostList posts)

retrieveSeriesList : Cmd Msg
retrieveSeriesList =
  Api.getSeries
    |> Task.mapError toString
    |> Task.perform Error (\series -> FromAdminBackend <| AdminSeriesList series)

retrieveUserList : Cmd Msg
retrieveUserList =
  getAdminUser
    |> Task.mapError toString
    |> Task.perform Error (\posts -> FromAdminBackend <| AdminUserList posts)

retrievePost : BlogTypes.BlogPostId -> Cmd Msg
retrievePost postId =
    Api.getPostById postId
        |> Task.mapError toString
        |> Task.perform Error (\post -> FromAdminBackend <| AdminPostDetail post)

retrieveSeries : SeriesId -> Cmd Msg
retrieveSeries seriesId =
  Api.getSeriesById seriesId
    |> Task.mapError toString
    |> Task.perform Error (\series -> FromAdminBackend <| AdminSeriesDetail series)

retrieveUser : UserId -> Cmd Msg
retrieveUser userId =
  Api.getUserById userId
    |> Task.mapError toString
    |> Task.perform Error (\user -> FromAdminBackend <| AdminUserDetail user)

deleteBlogPost : Api.BlogPost -> Cmd Msg
deleteBlogPost post =
  deleteAdminPostById post.bid
    |> Task.mapError toString
    |> Task.perform Error (\rr -> FromAdminBackend <| AdminResultResp rr)

deleteAuthor : Api.Author -> Cmd Msg
deleteAuthor author =
  deleteAdminUserById author.aid
    |> Task.mapError toString
    |> Task.perform Error (\rr -> FromAdminBackend <| AdminResultResp rr)

deleteSeries : Api.BlogSeries -> Cmd Msg
deleteSeries series =
  deleteAdminSeriesById series.sid
    |> Task.mapError toString
    |> Task.perform Error (\rr -> FromAdminBackend <| AdminResultResp rr)

createBlogPost : Api.BlogPost -> Cmd Msg
createBlogPost post =
  postAdminPost post
    |> Task.mapError toString
    |> Task.perform Error (\post -> FromAdminBackend <| AdminPostDetail post)

createAuthor : Api.Author -> Cmd Msg
createAuthor author =
  postAdminUser author
    |> Task.mapError toString
    |> Task.perform Error (\user -> FromAdminBackend <| AdminUserDetail user)

createSeries : Api.BlogSeries -> Cmd Msg
createSeries series =
  postAdminSeries series
    |> Task.mapError toString
    |> Task.perform Error (\series -> FromAdminBackend <| AdminSeriesDetail series)

editBlogPost : Api.BlogPost -> Cmd Msg
editBlogPost post =
  putAdminPostById post.bid post
    |> Task.mapError toString
    |> Task.perform Error (\rr -> FromAdminBackend <| AdminResultResp rr)

editAuthor : Author -> Cmd Msg
editAuthor author =
  putAdminUserById author.aid author
    |> Task.mapError toString
    |> Task.perform Error (\rr -> FromAdminBackend <| AdminResultResp rr)

editSeries : BlogSeries -> Cmd Msg
editSeries series =
  putAdminSeriesById series.sid series
    |> Task.mapError toString
    |> Task.perform Error (\rr -> FromAdminBackend <| AdminResultResp rr)
