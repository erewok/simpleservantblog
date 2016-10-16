module Blog.Api exposing (..)

import Date exposing (..)

import Exts.Date exposing (..)

import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Http
import String
import Task


type alias PostOverview =
  { pid : Int
  , ptitle : String
  , psynopsis : Maybe String
  , ppubdate : Maybe Date
  , pordinal : Maybe Int
  , pseriesid : Maybe Int
  , pseriesname : Maybe String
  , pseriesdescription : Maybe String
  }

decodePostOverview : Json.Decode.Decoder PostOverview
decodePostOverview =
  Json.Decode.succeed PostOverview
    |: ("pid" := Json.Decode.int)
    |: ("ptitle" := Json.Decode.string)
    |: ("psynopsis" := Json.Decode.maybe Json.Decode.string)
    |: ("ppubdate" := Json.Decode.maybe Json.Decode.Extra.date)
    |: ("pordinal" := Json.Decode.maybe Json.Decode.int)
    |: ("pseriesid" := Json.Decode.maybe Json.Decode.int)
    |: ("pseriesname" := Json.Decode.maybe Json.Decode.string)
    |: ("pseriesdescription" := Json.Decode.maybe Json.Decode.string)

getPost : Task.Task Http.Error (List (PostOverview))
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
      (Json.Decode.list decodePostOverview)
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

type alias PostSeries =
  { previous : List BlogPost
  , current : BlogPost
  , next : List BlogPost
  , series : BlogSeries
  }

type alias BlogSeries =
  { sid : Int
  , name : String
  , description : String
  , parentid : Maybe Int
  }

decodePostSeries : Json.Decode.Decoder PostSeries
decodePostSeries =
  Json.Decode.succeed PostSeries
    |: ("previous" := Json.Decode.list decodeBlogPost)
    |: ("current" := decodeBlogPost)
    |: ("next" := Json.Decode.list decodeBlogPost)
    |: ("series" := decodeBlogSeries)

decodeBlogSeries : Json.Decode.Decoder BlogSeries
decodeBlogSeries =
  Json.Decode.succeed BlogSeries
    |: ("sid" := Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("description" := Json.Decode.string)
    |: ("parentid" := Json.Decode.maybe Json.Decode.int)

getSeriesPostById : Int -> Task.Task Http.Error (PostSeries)
getSeriesPostById id =
  let
    request =
      { verb =
          "GET"
      , headers =
          [("Content-Type", "application/json")]
      , url =
          "/" ++ "series"
          ++ "/" ++ "post"
          ++ "/" ++ (id |> toString |> Http.uriEncode)
      , body =
          Http.empty
      }
  in
    Http.fromJson
      decodePostSeries
      (Http.send Http.defaultSettings request)

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
