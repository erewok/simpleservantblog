module Admin.AdminApi exposing (..)

import Date exposing (..)

import Exts.Date exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String


getAdminUser : Http.Request (List (Author))
getAdminUser =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "user"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeAuthor)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAdminUserById : Int -> Http.Request (Author)
getAdminUserById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "user"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeAuthor
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAdminUser : Author -> Http.Request (Author)
postAdminUser body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "user"
                ]
        , body =
            Http.jsonBody (encodeAuthor body)
        , expect =
            Http.expectJson decodeAuthor
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putAdminUserById : Int -> Author -> Http.Request (ResultResp)
putAdminUserById capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "user"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeAuthor body)
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteAdminUserById : Int -> Http.Request (ResultResp)
deleteAdminUserById capture_id =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "user"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAdminPost : BlogPost -> Http.Request (BlogPost)
postAdminPost body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "post"
                ]
        , body =
            Http.jsonBody (encodeBlogPost body)
        , expect =
            Http.expectJson decodeBlogPost
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putAdminPostById : Int -> BlogPost -> Http.Request (ResultResp)
putAdminPostById capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "post"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeBlogPost body)
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteAdminPostById : Int -> Http.Request (ResultResp)
deleteAdminPostById capture_id =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "post"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAdminPost : Http.Request (List (BlogPost))
getAdminPost =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "post"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson (list decodeBlogPost)
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAdminPostById : Int -> Http.Request (BlogPost)
getAdminPostById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
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

postAdminSeries : BlogSeries -> Http.Request (BlogSeries)
postAdminSeries body =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "series"
                ]
        , body =
            Http.jsonBody (encodeBlogSeries body)
        , expect =
            Http.expectJson decodeBlogSeries
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putAdminSeriesById : Int -> BlogSeries -> Http.Request (ResultResp)
putAdminSeriesById capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "series"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeBlogSeries body)
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteAdminSeriesById : Int -> Http.Request (ResultResp)
deleteAdminSeriesById capture_id =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "series"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

getAdminMedia : Http.Request (List (Media))
getAdminMedia =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "media"
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

getAdminMediaById : Int -> Http.Request (Media)
getAdminMediaById capture_id =
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "media"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeMedia
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAdminMedia : Http.Request (ResultResp)
postAdminMedia =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "media"
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

putAdminMediaById : Int -> Media -> Http.Request (ResultResp)
putAdminMediaById capture_id body =
    Http.request
        { method =
            "PUT"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "media"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.jsonBody (encodeMedia body)
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteAdminMediaById : Int -> Http.Request (ResultResp)
deleteAdminMediaById capture_id =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "media"
                , capture_id |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

postAdminPostMediaByPidByMid : Int -> Int -> Http.Request (ResultResp)
postAdminPostMediaByPidByMid capture_pid capture_mid =
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "post"
                , "media"
                , capture_pid |> toString |> Http.encodeUri
                , capture_mid |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }

deleteAdminPostMediaByPidByMid : Int -> Int -> Http.Request (ResultResp)
deleteAdminPostMediaByPidByMid capture_pid capture_mid =
    Http.request
        { method =
            "DELETE"
        , headers =
            []
        , url =
            String.join "/"
                [ ""
                , "admin"
                , "post"
                , "media"
                , capture_pid |> toString |> Http.encodeUri
                , capture_mid |> toString |> Http.encodeUri
                ]
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson decodeResultResp
        , timeout =
            Nothing
        , withCredentials =
            False
        }