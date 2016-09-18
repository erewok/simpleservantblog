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
        { init = init2
        , update = update2
        , subscriptions = \_ -> Sub.none
        , view = view2
        }

init : (Content, Cmd Msg)
init =
  let
    state =
      { content = PostList [], error = Nothing}
    in
      ( state, retrieve Home)

init2 : (BlogContent, Cmd Msg)
init2 =
  let
    state =
      { posts = PostList [], detail = Nothing }
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
            { s | content = PostList posts , error = Nothing } ! []
          Series series ->
            { s | content = Series series , error = Nothing } ! []
          PostDetail post ->
            { s | error = Nothing, content = PostDetail post } ! []
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

update2 : Msg -> BlogContent -> ( BlogContent, Cmd Msg )
update2 message s =
    case message of
      NoOp ->
        s ! []
      FromBackend backend ->
        case backend of
          PostList posts ->
            { s | posts = PostList posts
            , detail = Nothing } ! []
          Series series ->
            { s | posts = Series series
            , detail = Nothing } ! []
          PostDetail post ->
            { s | detail = Just post } ! []
          BackendError error ->
            { s | posts = BackendError error } ! []
      FromFrontend frontend ->
        case frontend of
          Home ->
            { s | posts = PostList [], detail = Nothing } ! [retrieve Home]
          SeePostList ->
            { s | posts = PostList [], detail = Nothing } ! [retrieve SeePostList]
          SeePostSeries seriesId ->
            { s | detail = Nothing } ! [retrieve (SeePostSeries seriesId)]
          SeePostDetail postId ->
            { s | detail = Nothing } ! [retrieve (SeePostDetail postId)]
          SeeAboutPage ->
            { s | detail = Nothing } ! [retrieve SeeAboutPage]
      Error msg ->
        { s | posts = BackendError msg } ! []


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

view2 : BlogContent -> Html Msg
view2 state = case state.detail of
  -- if there is a post-detail, show that
  Just post -> div [] [ pageSkeleton
      , div [ class "post-container row" ]
        <| [viewPost post]
      ]
  Nothing -> case state.posts of
    Series series -> div [] [ pageSkeleton
        , div [ class "post-container row" ]
            <| (seriesIndex series) ++ (List.map viewPostSummary series.posts)
        ]
    PostList posts -> div [] [ pageSkeleton
      , div [ class "post-container row" ]
          <| (List.map viewPostSummary posts)
      ]
    PostDetail post -> div [] [ pageSkeleton
      , div [ class "post-container row" ]
        <| [viewPost post]
      ]
    BackendError error -> div [] [pageSkeleton
      , div [ class "error row" ]
        <| [text "Something went wrong", text error]
      ]
