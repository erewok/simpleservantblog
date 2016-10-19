module Admin.Types exposing (..)

import Admin.AdminApi exposing (..)

type alias Model =
    { route : Route
    , user : String
    , content : Maybe Content
    , error : Maybe String
    }

type Msg
    = NoOp
    | GoToAdminMain
    | ServerList Content
    | ServerDetail Verb Content
    | SeeListContent Content
    | SeeDetailContent Content
    | Error String

type Editable
  = Inline
  | SeparatePage

type Content
  = ListPosts (List BlogPost)
  -- | ListSeries (List Series)
  | ListUsers (List Author)
  | DetailPost PostId
  | DetailSeries SeriesId
  | DetailUser UserId

type Verb
  = Get
  | Put
  | Post
  | Delete


type Route
    = AdminMain
    | AdminPostList
    | AdminUserList
    | AdminPostDetail PostId
    | AdminUserDetail UserId

type alias PostId = Int
type alias SeriesId = Int
type alias UserId = Int
