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
  { id : Int
  , title : String
  , synopsis : Maybe String
  , published : Bool
  , body : String
  , authorId : Int
  , created : Date
  , modified : Date
  , pubdate : Date
  }

decodeBlogPost : Json.Decode.Decoder BlogPost
decodeBlogPost =
  Json.Decode.succeed BlogPost
    |: ("id" := Json.Decode.int)
    |: ("title" := Json.Decode.string)
    |: ("synopsis" := Json.Decode.maybe Json.Decode.string)
    |: ("published" := Json.Decode.bool)
    |: ("body" := Json.Decode.string)
    |: ("authorId" := Json.Decode.int)
    |: ("created" := Json.Decode.Extra.date)
    |: ("modified" := Json.Decode.Extra.date)
    |: ("pubdate" := Json.Decode.Extra.date)

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
    [ ( "id", Json.Encode.int x.id )
    , ( "title", Json.Encode.string x.title )
    , ( "synopsis", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.synopsis )
    , ( "published", Json.Encode.bool x.published )
    , ( "body", Json.Encode.string x.body )
    , ( "authorId", Json.Encode.int x.authorId )
    , ( "created", (Json.Encode.string << Exts.Date.toISOString) x.created )
    , ( "modified", (Json.Encode.string << Exts.Date.toISOString) x.modified )
    , ( "pubdate", (Json.Encode.string << Exts.Date.toISOString) x.pubdate )
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

type alias Author =
  { id : Int
  , firstName : String
  , lastName : String
  , email : String
  }

encodeAuthor : Author -> Json.Encode.Value
encodeAuthor x =
  Json.Encode.object
    [ ( "id", Json.Encode.int x.id )
    , ( "firstName", Json.Encode.string x.firstName )
    , ( "lastName", Json.Encode.string x.lastName )
    , ( "email", Json.Encode.string x.email )
    ]

decodeAuthor : Json.Decode.Decoder Author
decodeAuthor =
  Json.Decode.succeed Author
    |: ("id" := Json.Decode.int)
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
