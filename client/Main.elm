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
-- import Page exposing (..)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- MODEL

type alias Content =
  { content : Backend
  , error : Maybe String
}

type Msg
    = NoOp
      | FromBackend Backend
      | FromFrontend Frontend
      | Error String

type Backend = PostList Posts
             | Series BlogSeriesWithPosts
             | PostDetail BlogPost
             | BackendError String

type Frontend = Home
                 | SeePostList
                 | SeePostSeries Int
                 | SeePostDetail Int
                 | SeeAboutPage

type alias Posts = List (BlogPost)

-- type alias PageComponents =
--   { header : Html a
--   , footer : Html a
--   , sidbar : Html a
--   , main : Content}


init : ( Content, Cmd Msg )
init =
    let
        state =
            { content = PostList [], error = Nothing }
    in
        ( state, retrieve Home)


-- UPDATE

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


--
-- toServer : (a -> Content ) -> Task Http.Error a -> Cmd Msg
-- toServer tag task =
--     perform (Error << toString) (FromFrontend << tag) task
--


-- VIEW

postsToMessage : Posts -> Msg
postsToMessage posts = FromBackend (PostList posts)


retrieve : Frontend -> Cmd Msg
retrieve frontendRequest = case frontendRequest of
  Home -> getAll
  SeePostList -> getAll
  SeePostSeries seriesId -> getAll
  SeePostDetail postId -> getAll
  SeeAboutPage -> getAboutPage

getAboutPage : Cmd Msg
getAboutPage = getAll

getAll : Cmd Msg
getAll = Api.getPost
  |> Task.mapError toString
  |> Task.perform Error postsToMessage


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
           (List.map viewPost posts)
    ]
  Series series -> div [] [ pageSkeleton
    , div [ class "post-container row" ]
        <| [text "this is a series of posts"] ++ []
    ]
  BackendError error -> div [] [pageSkeleton
    , div [ class "error row" ]
      <| [text "Something went wrong", text error]
    ]

fromJustStr : Maybe String -> String
fromJustStr someval = case someval of
  Nothing -> ""
  Just t -> t

fromJustDate : Maybe Date -> String
fromJustDate somedate = case somedate of
  Nothing -> ""
  Just date -> toString (day date) ++ " "
              ++ toString (month date) ++ " "
              ++ toString (year date)


viewPostSummary : BlogPost -> Html Msg
viewPostSummary post =  div [ class "post-content" ] (postTitleAndSynopsis post)


viewPost : BlogPost -> Html Msg
viewPost post =  div [ class "post-content" ] (postBody post)

postIndex : List (Html a)
postIndex = [ p [] [ text "This is a series of posts on constructing this blog. All posts are listed below."]
            , div [class "six columns "] [text ""]
            ]

postTitleAndSynopsis : BlogPost -> List (Html a)
postTitleAndSynopsis post = [(postTitle post)] ++
                            [ div [ class "row post-synopsis" ] [
                                p [] [ text (fromJustStr post.synopsis)]]
                            ]

postTitle : BlogPost -> Html a
postTitle post = div [ class "row post-title" ] [
                      h3 [ class "" ] [ text (post.title) ]
                    , span [ class "" ] [ text (fromJustDate post.pubdate)]
                    ]


postBody : BlogPost -> List (Html a)
postBody post = [ div [ class "row post-body" ] [
                    p [] [text (fromJustStr post.body)] ]
                ]


pageSkeleton : Html Msg
pageSkeleton = div [class "container" ] [
                div [class "row" ] [
                  div [class "three columns", style [("margin-top", "2%")]] [
                    a [onClick (FromFrontend Home), class "button", style [("border", "none")]] [
                      h5 [class "u-pull-right"] [text "EKADANTA"]
                    ]
                  ]
                , div [class "nine columns", style [("margin-top", "2%")]] [
                    div [class "top-nav"] [
                      div [class "two columns"] [
                        a [onClick (FromFrontend SeePostList), class "button" ] [text "Blog"]
                        ]
                    , div [class "one column"] [text ""]
                    , div [class "two columns"] [
                        a [onClick (FromFrontend SeeAboutPage), class "button" ] [text "About"]
                      ]
                      , div [class "one column"] [text ""]
                      , div [class "two columns"] [
                        a [class "button" ] [text "Projects"]
                      ]
                    ]
                  ]
                ]
              ]
