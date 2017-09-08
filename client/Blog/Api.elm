module Blog.Api exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String

import Blog.Types exposing (..)


getPost : Http.Request (List (PostOverview))
getPost =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "post"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodePostOverview)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getPostById : Int -> Http.Request (BlogPost)
getPostById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "post"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeBlogPost
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getPostMediaById : Int -> Http.Request (List (Media))
getPostMediaById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "post"
                , "media"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeMedia)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSeriesPostById : Int -> Http.Request (PostSeries)
getSeriesPostById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "series"
                , "post"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodePostSeries
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSeries : Http.Request (List (BlogSeries))
getSeries =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "series"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeBlogSeries)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getSeriesById : Int -> Http.Request (BlogSeries)
getSeriesById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "series"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeBlogSeries
        , timeout =
            Nothing
        , withCredentials =
            False
        }
