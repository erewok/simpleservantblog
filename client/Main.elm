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
import Navigation
import Api exposing (..)
import Post exposing (..)
import Series exposing (..)
import Types exposing (..)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    let
        state =
            { content = PostList [], error = Nothing }
    in
        ( state, retrieveAll )


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        NoOp ->
            s ! []

        FromBackend backend ->
            case backend of
                PostList posts ->
                    { s
                        | content = PostList posts
                        , error = Nothing
                    }
                        ! []

                SeriesPosts series ->
                    { s
                        | content = SeriesPosts series
                        , error = Nothing
                    }
                        ! []

                PostDetail post ->
                    { s
                        | content = PostDetail post
                        , error = Nothing
                    }
                        ! []

                BackendError msg ->
                    { s | content = BackendError msg, error = Just msg } ! []

        FromFrontend frontend ->
            case frontend of
                SeePostList ->
                    { s | content = PostList [], error = Nothing } ! [ retrieveAll ]

                SeePostDetail postId ->
                    { s | error = Nothing } ! [ retrievePost postId ]

                SeeSeriesPostDetail postId ->
                    { s | error = Nothing } ! [ retrieveSeriesPost postId ]

        Error msg ->
            { s | content = BackendError msg, error = Just msg } ! []



-- VIEW


retrieveAll : Cmd Msg
retrieveAll =
    Api.getPost
        |> Task.mapError toString
        |> Task.perform Error postsToMessage


retrieveSeriesPost : BlogPostId -> Cmd Msg
retrieveSeriesPost postId =
    Api.getSeriesPostById postId
        |> Task.mapError toString
        |> Task.perform Error seriesPostsToMessage


retrievePost : BlogPostId -> Cmd Msg
retrievePost postId =
    Api.getPostById postId
        |> Task.mapError toString
        |> Task.perform Error postToMessage


postsToMessage : List PostOverview -> Msg
postsToMessage posts =
    FromBackend (PostList posts)


postToMessage : BlogPost -> Msg
postToMessage post =
    FromBackend (PostDetail post)


seriesPostsToMessage : PostSeries -> Msg
seriesPostsToMessage series =
    FromBackend (SeriesPosts series)


view : Model -> Html Msg
view state =
    case state.content of
        -- if there is a post-detail, show that
        PostList posts ->
            div []
                [ div [ class "post-main row" ] <|
                    (List.map viewPostSummary posts)
                ]

        SeriesPosts seriesPost ->
            div []
                [ div [ class "post-main row" ] <|
                    [ viewSeriesPost seriesPost ]
                ]

        PostDetail post ->
            div []
                [ div [ class "post-main row" ] <|
                    [ viewPost post ]
                ]

        BackendError error ->
            div []
                [ div [ class "error row" ] <|
                    [ h4 [] [ text "Something went wrong" ]
                    , p [] [ text error ]
                    ]
                ]
