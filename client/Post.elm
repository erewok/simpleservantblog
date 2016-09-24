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


viewPostSummary : BlogPost -> Html Msg
viewPostSummary post =
    div [ class "post-content" ] (postTitleAndSynopsis post)


viewPost : BlogPost -> Html Msg
viewPost post =
    div [ class "post-content" ] (postBody post)


seriesIndex : SeriesDigest -> List (Html a)
seriesIndex series =
    [ div [ class "series-index" ]
        [ ol [] (List.map seriesIndexItem series.posts)
        ]
    ]


seriesIndexItem : SeriesDigest -> Html a
seriesIndexItem post =
    li [] [ a [ onClick (FromFrontend (SeePostDetail post.bid post.seriesId)), href "#" ] [ text post.title ] ]


postTitleAndSynopsis : BlogPost -> List (Html Msg)
postTitleAndSynopsis post =
    [ (postTitle post) ]
        ++ [ div [ class "row" ]
                [ M.toHtml [ class "post-synopsis" ] (fromJustStr post.synopsis)
                ]
           ]


postTitle : BlogPost -> Html Msg
postTitle post =
    div [ class "row" ]
        [ h3 [ class "post-title" ]
            [ a [ onClick (FromFrontend ( SeePostDetail post.bid, post.seriesId )), href "#" ] [ text (post.title) ]
            ]
        , span [ class "" ] [ text (fromJustDate post.pubdate) ]
        ]


postBody : BlogPost -> List (Html a)
postBody post =
    [ div [ class "row" ]
        [ M.toHtml [ class "post-body" ] (fromJustStr post.body)
        ]
    ]
