module Types exposing (..)

import Api exposing (..)


type alias Model =
    { content : Backend
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
    | SeeSeriesPostDetail BlogPostId



-- frontend other:
-- | SeeSeriesList


type ContentTypes
    = AllPosts
    | OnePost
    | OneSeriesPost


type alias BlogPostId =
    Int


type alias SeriesId =
    Maybe Int


type Msg
    = NoOp
    | FromBackend Backend
    | FromFrontend Frontend
    | Error String
