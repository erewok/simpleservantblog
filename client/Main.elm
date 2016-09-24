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


init : ( BlogContent, Cmd Msg )
init =
    let
        state =
            { posts = PostList [], detail = Nothing }
    in
        ( state, retrieve Home )


update : Msg -> BlogContent -> ( BlogContent, Cmd Msg )
update message s =
    case message of
        NoOp ->
            s ! []

        FromBackend backend ->
            case backend of
                PostList posts ->
                    { s
                        | posts = PostList posts
                        , detail = Nothing
                    }
                        ! []

                Series series ->
                    { s
                        | posts = Series series
                        , detail = Nothing
                    }
                        ! []

                PostDetail post ->
                    { s | detail = Just post } ! []

                BackendError error ->
                    { s | posts = BackendError error } ! []

        FromFrontend frontend ->
            case frontend of
                Home ->
                    { s | posts = PostList [], detail = Nothing } ! [ retrieve Home ]

                SeePostList ->
                    { s | posts = PostList [], detail = Nothing } ! [ retrieve SeePostList ]

                SeePostSeries seriesId ->
                    { s | detail = Nothing } ! [ retrieve (SeePostSeries seriesId) ]

                SeePostDetail postId ->
                    { s | detail = Nothing } ! [ retrieve (SeePostDetail postId) ]

        Error msg ->
            { s | posts = BackendError msg } ! []



-- VIEW


retrieve : Frontend -> Cmd Msg
retrieve frontendRequest =
    case frontendRequest of
        Home ->
            getAll

        SeePostList ->
            getAll

        SeePostSeries seriesId ->
            getSeries seriesId

        SeePostDetail postId ->
            getPost postId


postsToMessage : Posts -> Msg
postsToMessage posts =
    FromBackend (PostList posts)


postToMessage : BlogPost -> Msg
postToMessage post =
    FromBackend (PostDetail post)


seriesToMessage : BlogSeriesWithPosts -> Msg
seriesToMessage series =
    FromBackend (Series series)


getAll : Cmd Msg
getAll =
    Api.getPost
        |> Task.mapError toString
        |> Task.perform Error postsToMessage


getPost : Int -> Cmd Msg
getPost postId =
    Api.getPostById postId
        |> Task.mapError toString
        |> Task.perform Error postToMessage


getSeries : Int -> Cmd Msg
getSeries seriesId =
    Api.getPostSeriesById seriesId
        |> Task.mapError toString
        |> Task.perform Error seriesToMessage


view : BlogContent -> Html Msg
view state =
    case state.detail of
        -- if there is a post-detail, show that
        Just post ->
            div []
                [ div [ class "post-main row" ] <|
                    [ viewPost post ]
                ]

        Nothing ->
            case state.posts of
                Series series ->
                    div []
                        [ div [ class "post-main row" ] <|
                            (seriesIndex series)
                                ++ (List.map viewPostSummary series.posts)
                        ]

                PostList posts ->
                    div []
                        [ div [ class "post-main row" ] <|
                            (List.map viewPostSummary posts)
                        ]

                PostDetail post ->
                    div []
                        [ div [ class "post-main row" ] <|
                            [ viewPost post ]
                        ]

                BackendError error ->
                    div []
                        [ div [ class "error row" ] <|
                            [ text "Something went wrong", text error ]
                        ]
