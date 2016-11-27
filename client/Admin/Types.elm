module Admin.Types exposing (..)

import Date exposing (Date)

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
    | FromAdminFrontend AdminFrontend
    | Error String

type AdminBackend
    = AdminPostList (List Api.PostOverview)
    | AdminPostDetail BlogPost
    | AdminSeriesList (List Api.BlogSeries)
    | AdminSeriesDetail Api.BlogSeries
    | AdminUserList (List Author)
    | AdminUserDetail Author
    | AdminResultResp ResultResp
    | BackendError String

type AdminFrontend
    = AdminGetList ListThing
    | AdminGetDetail DetailThing
    | AdminDelete Item
    | AdminCreate Item
    | AdminEdit Item

type ListThing
  = ListPosts
  | ListUsers
  | ListSeries

type DetailThing
  = DetailPost PostId
  | DetailSeries SeriesId
  | DetailUser UserId

type Item
  = PI BlogPost
  | AI Author
  | SI BlogSeries

type Route
    = AdminMainR
    | AdminPostListR
    | AdminUserListR
    | AdminSeriesListR
    | AdminPostDetailR PostId
    | AdminUserDetailR UserId
    | AdminSeriesDetailR SeriesId

type alias PostId = Int
type alias SeriesId = Int
type alias UserId = Int
type alias AuthorId = Int
