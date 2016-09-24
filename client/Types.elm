module Types exposing (..)

import Api exposing (..)


type alias BlogContent =
    { posts : Backend
    , detail : Maybe BlogPost
    }


type Msg
    = NoOp
    | FromBackend Backend
    | FromFrontend Frontend
    | Error String


type Backend
    = PostList (List PostOverview)
    | SeriesDetail BlogSeries
    | SeriesPosts (List BlogPost)
    | PostDetail BlogPost
    | BackendError String


type alias BlogPostId =
    Int


type alias SeriesId =
    Int


type Frontend
    = Home
    | SeePostList
    | SeePostDetail BlogPostId SeriesId


type alias SeriesDigest =
    { previous : List BlogPost
    , current : BlogPost
    , next : List BlogPost
    , series : BlogSeries
    }
