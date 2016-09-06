module Api exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias BlogPost =
  { bid : Int
  , authorId : Int
  , title : String
  , body : Maybe String
  , published : Bool
  , created : Date
  , modified : Maybe Date
  , pubdate : Maybe Date
  , synopsis : Maybe String
  }

decodeBlogPost : Json.Decode.Decoder BlogPost
decodeBlogPost =
  Json.Decode.succeed BlogPost
    |: ("bid" := Json.Decode.int)
    |: ("authorId" := Json.Decode.int)
    |: ("title" := Json.Decode.string)
    |: ("body" := Json.Decode.maybe Json.Decode.string)
    |: ("published" := Json.Decode.bool)
    |: ("created" := Json.Decode.Extra.date)
    |: ("modified" := Json.Decode.maybe Json.Decode.Extra.date)
    |: ("pubdate" := Json.Decode.maybe Json.Decode.Extra.date)
    |: ("synopsis" := Json.Decode.maybe Json.Decode.string)

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
    , ( "title", Json.Encode.string x.title )
    , ( "body", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.body )
    , ( "published", Json.Encode.bool x.published )
    , ( "created", (Json.Encode.string << Exts.Date.toISOString) x.created )
    , ( "modified", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << Exts.Date.toISOString)) x.modified )
    , ( "pubdate", (Maybe.withDefault Json.Encode.null << Maybe.map (Json.Encode.string << Exts.Date.toISOString)) x.pubdate )
    , ( "synopsis", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.synopsis )
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