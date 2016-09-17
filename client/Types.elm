module Types exposing(..)

import Api exposing(..)


type alias Content =
  { content : Backend
  , error : Maybe String
}

type Msg
    = NoOp
      | FromBackend Backend
      | FromFrontend Frontend
      | Error String

type Backend = PostList Posts
             | Series BlogSeriesWithPosts
             | PostDetail BlogPost
             | BackendError String

type Frontend = Home
                 | SeePostList
                 | SeePostSeries Int
                 | SeePostDetail Int
                 | SeeAboutPage

type alias Posts = List (BlogPost)


-- type alias PageComponents =
--   { header : Html a
--   , footer : Html a
--   , sidbar : Html a
--   , main : Content}
