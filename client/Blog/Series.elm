module Blog.Series exposing (..)

import Date exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Exts.List as EL
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List as L
import List.Extra as LX
import Task exposing (Task, perform)

import Blog.Api exposing (..)
import Blog.Post exposing (..)
import Blog.Routes exposing (..)
import Blog.Types exposing (..)


-- Pick an arbitrarily large number so those without ordering are at the end.


comparingOrdinals : Maybe Int -> Int
comparingOrdinals val =
    case val of
        Nothing ->
            10000

        Just v ->
            v


updateFromCurrentSeries : BlogPostId -> PostSeries -> PostSeries
updateFromCurrentSeries postId seriesDigest =
    let
        after =
            L.sortBy (\p -> comparingOrdinals p.ordinal)
                (L.concat
                    [ seriesDigest.previous
                    , [ seriesDigest.current ]
                    , seriesDigest.next
                    ]
                )

        ( previous, rest ) =
            LX.span (\p -> p.bid /= postId) after

        current =
            case EL.firstMatch (\p -> p.bid == postId) rest of
                Nothing ->
                    seriesDigest.current

                Just post ->
                    post

        next =
            L.drop 1 rest

        updatedDigest =
            { series = seriesDigest.series
            , current = current
            , next = next
            , previous = previous
            }
    in
        updatedDigest


viewSeriesPost : PostSeries -> Html Msg
viewSeriesPost seriesDigest =
    div []
        [ div [ class "series-main" ]
            [ seriesInfo seriesDigest.series
            , seriesIndex seriesDigest
            , hr [] []
            ]
        , viewPost seriesDigest.current
        ]


seriesInfo : BlogSeries -> Html Msg
seriesInfo series =
    div [ class "series-info" ]
        [ h3 [] [ text series.name ]
        , p [] [ text series.description ]
        ]


seriesIndex : PostSeries -> Html Msg
seriesIndex seriesDigest =
    div [ class "series-index" ]
        [ h2 [] [ text "Index" ]
        , ol [] <|
            L.concat
                [ (L.map (seriesIndexItem seriesDigest.series.sid) seriesDigest.previous)
                , [ seriesIndexCurrent seriesDigest.current ]
                , (L.map (seriesIndexItem seriesDigest.series.sid) seriesDigest.next)
                ]
        ]


seriesIndexItem : SeriesId -> BlogPost -> Html Msg
seriesIndexItem seriesId post =
    li []
        [ a
            [ onClick (FromFrontend (SeeSeriesPostDetail post.bid seriesId))
            ]
            [ text post.title
            ]
        ]


seriesIndexCurrent : BlogPost -> Html Msg
seriesIndexCurrent post =
    li [] [ text post.title ]
