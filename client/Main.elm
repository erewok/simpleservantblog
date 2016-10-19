module Main exposing (..)

import Platform.Cmd exposing (none)
import Date exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (..)
import Task exposing (Task, perform, succeed)
import Navigation
import RouteUrl
import Blog.Api as Api
import Blog.Api exposing (..)
import Blog.Post exposing (..)
import Blog.Routes exposing (..)
import Blog.Series exposing (..)
import Blog.Types exposing (..)


main : Program Never
main =
    RouteUrl.program
        { delta2url = delta2url
        , location2messages = location2messages
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    let
        state =
            { content = PostList [], error = Nothing, route = HomeRoute }
    in
        ( state, retrieveAll )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NoOp ->
            model ! []

        FromBackend backend ->
            case backend of
                PostList posts ->
                    { model
                        | content = PostList posts
                        , error = Nothing
                        , route = HomeRoute
                    }
                        ! []

                SeriesPosts series ->
                    { model
                        | content = SeriesPosts series
                        , error = Nothing
                        , route = SeriesPostDetailRoute series.series.sid series.current.bid
                    }
                        ! []

                PostDetail post ->
                    { model
                        | content = PostDetail post
                        , error = Nothing
                        , route = PostDetailRoute post.bid
                    }
                        ! []

                BackendError msg ->
                    { model | content = BackendError msg, error = Just msg } ! []

        FromFrontend frontend ->
            case frontend of
                SeePostList ->
                    { model | content = PostList [], error = Nothing } ! [ retrieveAll ]

                SeePostDetail postId ->
                    { model | error = Nothing } ! [ retrievePost postId ]

                SeeSeriesPostDetail postId seriesId ->
                    case model.content of
                        SeriesPosts seriesDigest ->
                            if seriesId == seriesDigest.series.sid then
                                let
                                    newSeries =
                                        updateFromCurrentSeries postId seriesDigest

                                    msg =
                                        FromBackend (SeriesPosts newSeries)

                                    newModel =
                                        { model | content = SeriesPosts newSeries }
                                in
                                    update msg newModel
                            else
                                { model | error = Nothing } ! [ retrieveSeriesPost postId ]

                        _ ->
                            { model | error = Nothing } ! [ retrieveSeriesPost postId ]

        Error msg ->
            { model | content = BackendError msg, error = Just msg } ! []



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
