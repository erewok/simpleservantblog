module Main exposing (..)

import Date exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (Task, perform)
import Api exposing (..)
import Page exposing (..)
import Post exposing (..)
import Types exposing (..)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Content, Cmd Msg )
init =
    let
        state =
            { content = PostList [], error = Nothing }
    in
        ( state, retrieve Home)


update : Msg -> Content -> ( Content, Cmd Msg )
update message s =
    case message of
      NoOp ->
        s ! []
      FromBackend backend ->
        case backend of
          PostList posts ->
            { s | content = (PostList posts) , error = Nothing } ! []
          Series series ->
            { s | content = (Series series) , error = Nothing } ! []
          PostDetail post ->
            { s | content = (PostDetail post) , error = Nothing } ! []
          BackendError error ->
            { s | error = Just error } ! []
      FromFrontend frontend ->
        case frontend of
          Home ->
            { s | error = Nothing } ! [retrieve Home]
          SeePostList ->
            { s | error = Nothing } ! [retrieve SeePostList]
          SeePostSeries seriesId ->
            { s | error = Nothing } ! [retrieve (SeePostSeries seriesId)]
          SeePostDetail postId ->
            { s | error = Nothing } ! [retrieve (SeePostDetail postId)]
          SeeAboutPage ->
            { s | error = Nothing } ! [retrieve SeeAboutPage]
      Error msg ->
        { s | error = Just msg } ! []


-- VIEW
retrieve : Frontend -> Cmd Msg
retrieve frontendRequest = case frontendRequest of
  Home -> getAll
  SeePostList -> getAll
  SeePostSeries seriesId -> getSeries seriesId
  SeePostDetail postId -> getPost postId
  SeeAboutPage -> getAboutPage

getAboutPage : Cmd Msg
getAboutPage = getAll

postsToMessage : Posts -> Msg
postsToMessage posts = FromBackend (PostList posts)

postToMessage : BlogPost -> Msg
postToMessage post = FromBackend (PostDetail post)

seriesToMessage : BlogSeriesWithPosts -> Msg
seriesToMessage series = FromBackend (Series series)


getAll : Cmd Msg
getAll = Api.getPost
  |> Task.mapError toString
  |> Task.perform Error postsToMessage


getPost : Int -> Cmd Msg
getPost postId = Api.getPostById postId
  |> Task.mapError toString
  |> Task.perform Error postToMessage


getSeries : Int -> Cmd Msg
getSeries seriesId = Api.getPostSeriesById seriesId
  |> Task.mapError toString
  |> Task.perform Error seriesToMessage


view : Content -> Html Msg
view state = case state.content of
  PostDetail post -> div [] [ pageSkeleton
    , div [ class "post-container row" ]
      <| [text "this is a post detail"] ++
         [viewPost post]
    ]
  PostList posts -> div [] [ pageSkeleton
    , div [ class "post-container row" ]
        <| [text "this is a post list"] ++
           (List.map viewPostSummary posts)
    ]
  Series series -> div [] [ pageSkeleton
    , div [ class "post-container row" ]
        <| [text "this is a series of posts"] ++ []
    ]
  BackendError error -> div [] [pageSkeleton
    , div [ class "error row" ]
      <| [text "Something went wrong", text error]
    ]
