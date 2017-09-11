module Blog.Types exposing (..)

import Date exposing (..)
import Exts.Date exposing (..)
import Json.Encode
import Json.Decode exposing (..)
import Json.Decode.Extra exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional)


type alias Model =
    { route : Route
    , content : Backend
    , error : Maybe String
    }


type Backend
    = PostList (List PostOverview)
    | SeriesPosts PostSeries
    | PostDetail BlogPost
    | BackendError String


type Frontend
    = SeePostList
    | SeePostDetail BlogPostId
    | SeeSeriesPostDetail BlogPostId SeriesId



-- frontend other:
-- | SeeSeriesList


type alias BlogPostId =
    Int


type alias SeriesId =
    Int


type Msg
    = NoOp
    | FromBackend Backend
    | FromFrontend Frontend
    | Error String


type Route
    = HomeRoute
    | PostDetailRoute BlogPostId
    | SeriesPostDetailRoute SeriesId BlogPostId


-- API Types

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



decodePostOverview : Decoder PostOverview
decodePostOverview =
    decode PostOverview
        |> required "pid" int
        |> required "ptitle" string
        |> required "psynopsis" (nullable string)
        |> required "ppubdate" (nullable date)
        |> required "pordinal" (nullable int)
        |> required "pseriesid" (nullable int)
        |> required "pseriesname" (nullable string)
        |> required "pseriesdescription" (nullable string)

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

decodeBlogPost : Decoder BlogPost
decodeBlogPost =
  decode BlogPost
    |> required "bid" int
    |> required "authorId" int
    |> required "seriesId" (nullable int)
    |> required "title" string
    |> required "body" (nullable string)
    |> required "synopsis" (nullable string)
    |> required "created" date
    |> required "modified" (nullable date)
    |> required "pubdate" (nullable date)
    |> required "ordinal" (nullable int)

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

decodePostSeries : Decoder PostSeries
decodePostSeries =
  decode PostSeries
    |> required "previous" (list decodeBlogPost)
    |> required "current" decodeBlogPost
    |> required "next" (list decodeBlogPost)
    |> required "series" decodeBlogSeries

decodeBlogSeries : Decoder BlogSeries
decodeBlogSeries =
  decode BlogSeries
    |> required "sid" int
    |> required "name" string
    |> required "description" string
    |> required "parentid" (nullable int)


type alias Author =
  { aid : Int
  , userid : Int
  , firstName : String
  , lastName : String
  }

decodeAuthor : Decoder Author
decodeAuthor =
  decode Author
    |> required "aid" int
    |> required "userid" int
    |> required "firstName" string
    |> required "lastName" string

type alias Media =
  { mediaid : Int
  , name : String
  , url : String
  , location : String
  , description : String
  }

decodeMedia : Decoder Media
decodeMedia =
  decode Media
    |> required "mediaid" int
    |> required "name" string
    |> required "url" string
    |> required "location" string
    |> required "description" string
