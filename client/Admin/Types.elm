module Admin.Types exposing (..)

import Admin.AdminApi exposing (..)
import Blog.Api as Api

type alias Model =
    { route : Route
    , user : String
    , content : Maybe AdminBackend
    }

type Msg
    = NoOp
    | GoToAdminMain
    | FromAdminBackend AdminBackend
    | FromAdminFrontend Frontend
    | Error String

type AdminBackend
    = AdminPostList (List BlogPost)
    | AdminPostDetail BlogPost
    | AdminSeriesList (List Api.BlogSeries)
    | AdminSeriesDetail Api.BlogSeries
    | AdminUserList (List Author)
    | AdminUserDetail Author
    | AdminResultResp ResultResp
    | BackendError String

type Frontend
    = AdminGetList ListThing
    | AdminGetDetail DetailThing
    | AdminDelete Item
    | AdminCreate Item
    | AdminEdit Item

type ListThing
  = ListPosts
  | ListUsers
  | ListSeries (List Api.BlogSeries)

type DetailThing
  = DetailPost PostId
  | DetailSeries SeriesId
  | DetailUser UserId

type Item
  = BlogPost
  | Author
  | Series

type Route
    = AdminMainR
    | AdminPostListR
    | AdminUserListR
    | AdminPostDetailR PostId
    | AdminUserDetailR UserId

type Editable
  = Inline
  | SeparatePage

type alias PostId = Int
type alias SeriesId = Int
type alias UserId = Int
