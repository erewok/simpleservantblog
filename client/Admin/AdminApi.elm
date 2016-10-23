module Admin.AdminApi exposing (..)

import Date exposing (..)

import Exts.Date exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias Author =
  { aid : Int
  , firstName : String
  , lastName : String
  , email : String
  }

decodeAuthor : Json.Decode.Decoder Author
decodeAuthor =
  Json.Decode.succeed Author
    |: ("aid" := Json.Decode.int)
    |: ("firstName" := Json.Decode.string)
    |: ("lastName" := Json.Decode.string)
    |: ("email" := Json.Decode.string)

getAdminUser : Task.Task Http.Error (List (Author))
getAdminUser =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "user"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeAuthor)
      (Http.send Http.defaultSettings request)

encodeAuthor : Author -> Json.Encode.Value
encodeAuthor x =
  Json.Encode.object
    [ ( "aid", Json.Encode.int x.aid )
    , ( "firstName", Json.Encode.string x.firstName )
    , ( "lastName", Json.Encode.string x.lastName )
    , ( "email", Json.Encode.string x.email )
    ]

postAdminUser : Author -> Task.Task Http.Error (Author)
postAdminUser body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "user"
      , body =
          Http.string (Json.Encode.encode 0 (encodeAuthor body))
      }
  in
    Http.fromJson
      decodeAuthor
      (Http.send Http.defaultSettings request)

type alias ResultResp =
  { status : String
  , description : String
  }

decodeResultResp : Json.Decode.Decoder ResultResp
decodeResultResp =
  Json.Decode.succeed ResultResp
    |: ("status" := Json.Decode.string)
    |: ("description" := Json.Decode.string)

putAdminUserById : Int -> Author -> Task.Task Http.Error (ResultResp)
putAdminUserById id body =
  let
    request =
      { verb =
          "PUT"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "user"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.string (Json.Encode.encode 0 (encodeAuthor body))
      }
  in
    Http.fromJson
      decodeResultResp
      (Http.send Http.defaultSettings request)

deleteAdminUserById : Int -> Task.Task Http.Error (ResultResp)
deleteAdminUserById id =
  let
    request =
      { verb =
          "DELETE"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "user"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeResultResp
      (Http.send Http.defaultSettings request)

type alias BlogPost =
  { bid : Int
  , authorId : Int
  , seriesId : Maybe Int
  , title : String
  , body : Maybe String
  , synopsis : Maybe String
  , created : Date
  , modified : Maybe Date
  , pubdate : Maybe Date
  , ordinal : Maybe Int
  }

encodeBlogPost : BlogPost -> Json.Encode.Value
encodeBlogPost x =
  Json.Encode.object
    [ ( "bid", Json.Encode.int x.bid )
    , ( "authorId", Json.Encode.int x.authorId )
    , ( "seriesId", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.seriesId )
    , ( "title", Json.Encode.string x.title )
    , ( "body", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.body )
    , ( "synopsis", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.synopsis )
    , ( "created", (Json.Encode.string << Exts.Date.toISOString) x.created )
    , ( "modified", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << Exts.Date.toISOString)) x.modified )
    , ( "pubdate", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << Exts.Date.toISOString)) x.pubdate )
    , ( "ordinal", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.ordinal )
    ]

decodeBlogPost : Json.Decode.Decoder BlogPost
decodeBlogPost =
  Json.Decode.succeed BlogPost
    |: ("bid" := Json.Decode.int)
    |: ("authorId" := Json.Decode.int)
    |: ("seriesId" := Json.Decode.maybe Json.Decode.int)
    |: ("title" := Json.Decode.string)
    |: ("body" := Json.Decode.maybe Json.Decode.string)
    |: ("synopsis" := Json.Decode.maybe Json.Decode.string)
    |: ("created" := Json.Decode.Extra.date)
    |: ("modified" := Json.Decode.maybe Json.Decode.Extra.date)
    |: ("pubdate" := Json.Decode.maybe Json.Decode.Extra.date)
    |: ("ordinal" := Json.Decode.maybe Json.Decode.int)

postAdminPost : BlogPost -> Task.Task Http.Error (BlogPost)
postAdminPost body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "post"
      , body =
          Http.string (Json.Encode.encode 0 (encodeBlogPost body))
      }
  in
    Http.fromJson
      decodeBlogPost
      (Http.send Http.defaultSettings request)

putAdminPostById : Int -> BlogPost -> Task.Task Http.Error (ResultResp)
putAdminPostById id body =
  let
    request =
      { verb =
          "PUT"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "post"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.string (Json.Encode.encode 0 (encodeBlogPost body))
      }
  in
    Http.fromJson
      decodeResultResp
      (Http.send Http.defaultSettings request)

deleteAdminPostById : Int -> Task.Task Http.Error (ResultResp)
deleteAdminPostById id =
  let
    request =
      { verb =
          "DELETE"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "post"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeResultResp
      (Http.send Http.defaultSettings request)

type alias BlogSeries =
  { sid : Int
  , name : String
  , description : String
  , parentid : Maybe Int
  }

encodeBlogSeries : BlogSeries -> Json.Encode.Value
encodeBlogSeries x =
  Json.Encode.object
    [ ( "sid", Json.Encode.int x.sid )
    , ( "name", Json.Encode.string x.name )
    , ( "description", Json.Encode.string x.description )
    , ( "parentid", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.int) x.parentid )
    ]

decodeBlogSeries : Json.Decode.Decoder BlogSeries
decodeBlogSeries =
  Json.Decode.succeed BlogSeries
    |: ("sid" := Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("description" := Json.Decode.string)
    |: ("parentid" := Json.Decode.maybe Json.Decode.int)

postAdminSeries : BlogSeries -> Task.Task Http.Error (BlogSeries)
postAdminSeries body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "series"
      , body =
          Http.string (Json.Encode.encode 0 (encodeBlogSeries body))
      }
  in
    Http.fromJson
      decodeBlogSeries
      (Http.send Http.defaultSettings request)

putAdminSeriesById : Int -> BlogSeries -> Task.Task Http.Error (ResultResp)
putAdminSeriesById id body =
  let
    request =
      { verb =
          "PUT"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "series"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.string (Json.Encode.encode 0 (encodeBlogSeries body))
      }
  in
    Http.fromJson
      decodeResultResp
      (Http.send Http.defaultSettings request)

deleteAdminSeriesById : Int -> Task.Task Http.Error (ResultResp)
deleteAdminSeriesById id =
  let
    request =
      { verb =
          "DELETE"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "admin"
          ++ "/" ++ "series"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeResultResp
      (Http.send Http.defaultSettings request)