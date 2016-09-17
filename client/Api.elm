module Api exposing (..)

import Date exposing (..)

import Exts.Date exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


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

getPost : Task.Task Http.Error (List (BlogPost))
getPost =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "post"
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeBlogPost)
      (Http.send Http.defaultSettings request)

getPostById : Int -> Task.Task Http.Error (BlogPost)
getPostById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "post"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeBlogPost
      (Http.send Http.defaultSettings request)

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

postPost : BlogPost -> Task.Task Http.Error (BlogPost)
postPost body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "post"
      , body =
          Http.string (Json.Encode.encode 0 (encodeBlogPost body))
      }
  in
    Http.fromJson
      decodeBlogPost
      (Http.send Http.defaultSettings request)

type alias BlogSeriesWithPosts =
  { bsid : Int
  , sname : String
  , sdescription : String
  , parent : Maybe Int
  , posts : List BlogPost
  }

decodeBlogSeriesWithPosts : Json.Decode.Decoder BlogSeriesWithPosts
decodeBlogSeriesWithPosts =
  Json.Decode.succeed BlogSeriesWithPosts
    |: ("bsid" := Json.Decode.int)
    |: ("sname" := Json.Decode.string)
    |: ("sdescription" := Json.Decode.string)
    |: ("parent" := Json.Decode.maybe Json.Decode.int)
    |: ("posts" := Json.Decode.list decodeBlogPost)

getPostSeriesById : Int -> Task.Task Http.Error (BlogSeriesWithPosts)
getPostSeriesById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "post"
          ++ "/" ++ "series"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodeBlogSeriesWithPosts
      (Http.send Http.defaultSettings request)

type alias Author =
  { aid : Int
  , firstName : String
  , lastName : String
  , email : String
  }

encodeAuthor : Author -> Json.Encode.Value
encodeAuthor x =
  Json.Encode.object
    [ ( "aid", Json.Encode.int x.aid )
    , ( "firstName", Json.Encode.string x.firstName )
    , ( "lastName", Json.Encode.string x.lastName )
    , ( "email", Json.Encode.string x.email )
    ]

decodeAuthor : Json.Decode.Decoder Author
decodeAuthor =
  Json.Decode.succeed Author
    |: ("aid" := Json.Decode.int)
    |: ("firstName" := Json.Decode.string)
    |: ("lastName" := Json.Decode.string)
    |: ("email" := Json.Decode.string)

postUser : Author -> Task.Task Http.Error (List (Author))
postUser body =
  let
    request =
      { verb =
          "POST"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "user"
      , body =
          Http.string (Json.Encode.encode 0 (encodeAuthor body))
      }
  in
    Http.fromJson
      (Json.Decode.list decodeAuthor)
      (Http.send Http.defaultSettings request)

getUserByFirstName : String -> Task.Task Http.Error (List (Author))
getUserByFirstName firstName =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "user"
          ++ "/" ++ (firstName |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeAuthor)
      (Http.send Http.defaultSettings request)

getUserByLastName : String -> Task.Task Http.Error (List (Author))
getUserByLastName lastName =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "user"
          ++ "/" ++ (lastName |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      (Json.Decode.list decodeAuthor)
      (Http.send Http.defaultSettings request)