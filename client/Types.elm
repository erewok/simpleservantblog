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


type Frontend
    = Home
    | SeePostList
    | SeePostDetail Int
