module Series exposing (..)

import Date exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (concat)
import Task exposing (Task, perform)
import Post exposing (..)
import Api exposing (..)
import Types exposing (..)


viewSeriesPost : PostSeries -> Html Msg
viewSeriesPost seriesPost =
    div []
        [ seriesInfo seriesPost.series
        , seriesIndex seriesPost.previous seriesPost.current seriesPost.next
        , hr [] []
        , viewPost seriesPost.current
        ]


seriesInfo : BlogSeries -> Html Msg
seriesInfo series =
    div [ class "series-info" ]
        [ h3 [] [ text series.name ]
        , p [] [ text series.description ]
        ]


seriesIndex : List BlogPost -> BlogPost -> List BlogPost -> Html Msg
seriesIndex prev current next =
    div [ class "series-index" ]
        [ h2 [] [ text "Index" ]
        , ol [] <|
            concat
                [ (List.map seriesIndexItem prev)
                , [ seriesIndexCurrent current ]
                , (List.map seriesIndexItem next)
                ]
        ]


seriesIndexItem : BlogPost -> Html Msg
seriesIndexItem post =
    li [] [ a [ onClick (FromFrontend (SeeSeriesPostDetail post.bid)), href "#" ] [ text post.title ] ]


seriesIndexCurrent : BlogPost -> Html Msg
seriesIndexCurrent post =
    li [] [ text post.title ]
