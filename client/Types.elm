module Types exposing (..)

import Api exposing (..)


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
    | Navigate String
    | Error String


type Route
    = HomeRoute
    | PostDetailRoute BlogPostId
    | SeriesPostDetailRoute BlogPostId SeriesId
