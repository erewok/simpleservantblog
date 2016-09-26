module Post exposing (..)

import Date exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Markdown as M
import Api exposing (..)
import Types exposing (..)


fromJustStr : Maybe String -> String
fromJustStr someval =
    case someval of
        Nothing ->
            ""

        Just t ->
            t


fromJustDate : Maybe Date -> String
fromJustDate somedate =
    case somedate of
        Nothing ->
            ""

        Just date ->
            toString (day date)
                ++ " "
                ++ toString (month date)
                ++ " "
                ++ toString (year date)


viewPostSummary : PostOverview -> Html Msg
viewPostSummary po =
    div [ class "post-content" ] (postTitleAndSynopsis po)


postTitleAndSynopsis : PostOverview -> List (Html Msg)
postTitleAndSynopsis po =
    [ (postTitle po) ]
        ++ [ div [ class "row" ]
                [ M.toHtml [ class "post-synopsis" ] (fromJustStr po.psynopsis)
                ]
           ]


postTitle : PostOverview -> Html Msg
postTitle po =
    case po.pseriesid of
        Nothing ->
            div [ class "row" ]
                [ h3 [ class "post-title" ]
                    [ a [ onClick (FromFrontend (SeePostDetail po.pid)), href "#" ] [ text (po.ptitle) ]
                    ]
                , span [ class "" ] [ text (fromJustDate po.ppubdate) ]
                ]

        Just seriesid ->
            div [ class "row" ]
                [ h3 [ class "post-title" ]
                    [ a [ onClick (FromFrontend (SeeSeriesPostDetail po.pid)), href "#" ] [ text (po.ptitle) ]
                    ]
                , span [ class "" ] [ text (fromJustDate po.ppubdate) ]
                ]


viewPost : BlogPost -> Html Msg
viewPost post =
    div [ class "post-content" ] (postBody post)


postBody : BlogPost -> List (Html a)
postBody post =
    [ div [ class "row" ]
        [ M.toHtml [ class "post-body" ] (fromJustStr post.body)
        ]
    ]
