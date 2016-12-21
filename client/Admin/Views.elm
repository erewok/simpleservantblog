module Admin.Views exposing (..)

import Date exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import List exposing (..)
import Markdown as M
import String exposing (..)
import Task exposing (Task, perform, succeed)

import Admin.AdminApi exposing (..)
import Admin.Routes exposing (..)
import Admin.Types exposing (..)
import Blog.Api as Api
import Blog.Types as BlogTypes
import Blog.Post as BlogViews

view : Model -> Html Msg
view state =
  div [] [
    header [ id "page-header", class "top-nav"] [
      div [ class "row" ] [
        div [ class "two columns", style [("margin-top", "2%")] ] [
          a [ onClick GoToAdminMain, class "button admin-button" ]
            [ text <| "Hi, ", (text state.user) ]
          ]
        , div [ class "two columns", style [("margin-top", "2%")] ] [
          a [ onClick (FromAdminFrontend <| AdminGetList ListUsers), class "button admin-button"]
            [ text "Users" ]
          ]
        , div [ class "two columns", style [("margin-top", "2%")] ] [
            a [ onClick (FromAdminFrontend <| AdminGetList ListSeries), class "button admin-button" ]
              [ text "Series" ]
          ]
        , div [ class "two columns", style [("margin-top", "2%")] ] [
            a [ onClick (FromAdminFrontend <| AdminGetList ListPosts), class "button admin-button" ]
              [ text "Posts" ]
          ]
        , div [ class "two columns", style [("margin-top", "2%")] ] [
            a [ class "button admin-button", href "/"]
              [ text "View Site" ]
          ]
        ]
      ]
      , viewState state.content
  ]

viewState : Maybe AdminBackend -> Html Msg
viewState content =
  case content of
    Nothing ->
      div [ class "admin-home" ] [
        p [] [ text " Welcome to admin"]
      ]
    Just (BackendError error) ->
      div [ class "admin-error" ] [
        p [] [ text " There was an error with your request"]
        , p [] [ (text error) ]
      ]
    Just (AdminPostList posts) ->
      adminPostsTable posts
    Just (AdminPostDetail post) ->
      adminPostEdit post
    Just (AdminSeriesList series) ->
      adminSeriesTable series
    Just (AdminSeriesDetail series) ->
      adminSeriesEdit series
    Just (AdminUserList authors) ->
      adminAuthorTable authors
    Just (AdminUserDetail author) ->
      adminAuthorEdit author
    _ ->
      div [ class "admin-home" ] [
        p [] [ text " Welcome to admin"]
      ]

adminPostsTable : List (BlogPost) -> Html Msg
adminPostsTable posts =
  div [ class "admin-post-table" ] [
    addNew <| PI newBlankPost
    , table [] [
      thead [] [
        tr [] [
          th [] [ text "Id" ]
          , th [] [ text "Title" ]
          , th [] [ text "Pubdate" ]
          , th [] [ text "Order" ]
          , th [] [ text "Series Id" ]
          , th [] [ text "Series Name" ]
        ]
      ]
      , tbody [] <| List.map viewPostInTable posts
    ]
  ]

viewPostInTable : BlogPost -> Html Msg
viewPostInTable post =
  tr [ class "admin-editable"
      , onClick (FromAdminFrontend
                  <| AdminGetDetail
                  <| DetailPost post.bid) ] [
    th [] [ text (toString post.bid) ]
    , th [] [ text post.title ]
    , th [] [ text (BlogViews.fromJustDate post.pubdate) ]
    , th [] [ text (toString post.ordinal) ]
    , th [] [ text (toString post.seriesId) ]
  ]

adminSeriesTable : List (Api.BlogSeries) -> Html Msg
adminSeriesTable series =
  div [ class "admin-post-table" ] [
    addNew <| SI newBlankSeries
    , table [] [
      thead [] [
        tr [] [
          th [] [ text "Id" ]
          , th [] [ text "Name" ]
          , th [] [ text "Description" ]
          , th [] [ text "Parent Id" ]

        ]
      ]
      , tbody [] <| List.map viewSeriesInTable series
    ]
  ]

viewSeriesInTable : Api.BlogSeries -> Html Msg
viewSeriesInTable series =
  tr [ class "admin-editable"
      , onClick (FromAdminFrontend
                  <| AdminGetDetail
                  <| DetailSeries series.sid) ] [
    th [] [ text (toString series.sid) ]
    , th [] [ text series.name ]
    , th [] [ text series.description ]
    , th [] [ text (toString series.parentid) ]
  ]

adminAuthorTable : List (Api.Author) -> Html Msg
adminAuthorTable authors =
  div [ class "admin-post-table" ] [
    table [] [
      thead [] [
        tr [] [
          th [] [ text "Id" ]
          , th [] [ text "First Name" ]
          , th [] [ text "Last Name" ]
        ]
      ]
      , tbody [] <| List.map viewAuthorInTable authors
    ]
  ]

viewAuthorInTable : Api.Author -> Html Msg
viewAuthorInTable author =
  tr [ class "admin-editable"
      , onClick (FromAdminFrontend
                  <| AdminGetDetail
                  <| DetailUser author.aid) ] [
      th [] [ text (toString author.aid) ]
    , th [] [ text author.firstName ]
    , th [] [ text author.lastName ]
  ]


addNew : Item -> Html Msg
addNew addType =
  div [ class "row", onClick <| FromAdminFrontend <| AdminCreate addType ] [
  a [ href "#" ] [
      div [class "two columns" ] [
        span [ class "add-new" ] [ text "+" ]
      ]
    ]
  ]

adminPostEdit : Api.BlogPost -> Html Msg
adminPostEdit post =
  div [ class "edit-main" ] [
    div [ class "row" ] [
      div [ class "eight columns" ] [
          p [] [ b [] [ text "Id: " ], text (toString post.bid) ]
          , p [] [ text ("Created: " ++ (BlogViews.dateToString post.created)) ]
          , label [ for "title" ] [ text "title" ]
          , input [ id "title"
                  , type' "text"
                  , placeholder "title"
                  , onInput (updatePostTitle post)
                  , value post.title ] []
          , label [ for "pubdate" ] [ text "pubdate" ]
          , input [ id "pubdate"
                  , type' "text"
                  , placeholder "pubdate"
                  , onInput (updatePostPublished post)
                  , value (BlogViews.fromJustDate post.pubdate) ] []
          , label [ for "seriesId" ] [ text "Series Id" ]
          , input [ id "seriesId"
                  , type' "text"
                  , placeholder "series id"
                  , onInput (updateSeriesId post)
                  , value (BlogViews.fromJustIntStr post.seriesId) ] []
          , label [ for "ordinal" ] [ text "Ordering" ]
          , input [ id "ordinal"
                  , type' "text"
                  , placeholder "ordering"
                  , onInput (updateOrdering post)
                  , value (BlogViews.fromJustIntStr post.ordinal) ] []
          ]
    , div [ class "four columns" ] [
      a [ class "button button-primary"
          , onClick <| FromAdminFrontend <| AdminEdit <| PI post
        ] [ text "Save" ]
      ]
    ]
  , hr [] []
  , div [class "row" ] [
      div [class "six columns" ] [
        textarea [ style [("width", "100%")]
                 , placeholder "Body"
                 , class "post-synopsis-input"
                 , onInput (updatePostSynopsis post) ] [
                    text (BlogViews.fromJustStr post.synopsis)
                ]
      ]
      , div [class "six columns" ] [
        M.toHtml [ class "post-synopsis" ]  (BlogViews.fromJustStr post.synopsis)
      ]
    ]
  , hr [] []
  , div [class "row" ] [
      div [ class "six columns" ] [
        textarea [ style [("width", "100%") ]
                 , placeholder "Body"
                 , class "post-body-input"
                 , onInput (updatePostBody post) ] [
                    text (BlogViews.fromJustStr post.body)
                ]
      ]
    , div [ class "six columns" ] [
        M.toHtml [ class "post-body" ] (BlogViews.fromJustStr post.body)
      ]
    ]
  ]

adminSeriesEdit : Api.BlogSeries -> Html Msg
adminSeriesEdit series =
  div [ class "edit-main" ] [
    div [ class "row" ] [
      div [ class "eight columns" ] [
        p [] [ b [] [ text "Id: " ], text (toString series.sid) ]
        , label [ for "name" ] [ text "name" ]
        , input [ id "name"
                , style [("width", "100%")]
                , type' "text"
                , placeholder "name"
                , onInput (updateSeriesName series)
                , value series.name ] []
       , label [ for "description" ] [ text "description" ]
       , input [ id "description"
               , style [("width", "100%")]
               , type' "text"
               , placeholder "description"
               , onInput (updateSeriesDescription series)
               , value series.description ] []
      ]
      , div [ class "four columns" ] [
        a [ class "button button-primary"
            , onClick <| FromAdminFrontend <| AdminEdit <| SI series
          ] [ text "Save" ]
        ]
      ]
    ]

adminAuthorEdit : Api.Author -> Html Msg
adminAuthorEdit author =
  div [ class "edit-main" ] [ text "edit author" ]

retrieveList : ListThing -> Cmd Msg
retrieveList listRequested = case listRequested of
  ListPosts -> getAdminPost
    |> Task.mapError toString
    |> Task.perform Error (\posts -> FromAdminBackend <| AdminPostList posts)
  ListSeries -> Api.getSeries
    |> Task.mapError toString
    |> Task.perform Error (\series -> FromAdminBackend <| AdminSeriesList series)
  ListUsers -> getAdminUser
    |> Task.mapError toString
    |> Task.perform Error (\posts -> FromAdminBackend <| AdminUserList posts)

createItem : Item -> Cmd Msg
createItem item = case item of
  PI post -> postAdminPost post |> postDetailResponse
  AI author -> postAdminUser author |> userDetailResponse
  SI series -> postAdminSeries series |> seriesDetailResponse

retrievePost : BlogTypes.BlogPostId -> Cmd Msg
retrievePost postId = getAdminPostById postId |> postDetailResponse

retrieveSeries : SeriesId -> Cmd Msg
retrieveSeries seriesId = Api.getSeriesById seriesId |> seriesDetailResponse

retrieveUser : UserId -> Cmd Msg
retrieveUser userId = getAdminUserById userId |> userDetailResponse

deleteItem : Item -> Cmd Msg
deleteItem item = case item of
  PI post -> deleteAdminPostById post.bid |> genericResponse
  AI author -> deleteAdminUserById author.aid |> genericResponse
  SI series -> deleteAdminSeriesById series.sid |> genericResponse

editItem : Item -> Cmd Msg
editItem item = case item of
  PI post -> putAdminPostById post.bid post |> genericResponse
  AI author -> putAdminUserById author.aid author |> genericResponse
  SI series -> putAdminSeriesById series.sid series |> genericResponse

--
-- Create new versions of items to post to backend --
--

-- When creating a new post, the ID and the created datetime will be ignored,
-- so we send nonsense values in
newBlankPost : BlogPost
newBlankPost = { bid = 0
              , authorId = 1
              , seriesId = Nothing
              , title = "New Post"
              , body = Nothing
              , synopsis = Nothing
              , created = fromTime 0
              , modified = Nothing
              , pubdate = Nothing
              , ordinal = Nothing
              }

-- When creating a new series, the ID will be ignored
newBlankSeries : BlogSeries
newBlankSeries = { sid = 0
                , name = "New Series"
                , description = "Series Description"
                , parentid = Nothing}

-- When creating a new author, the ID will be ignored
newBlankAuthor : Author
newBlankAuthor  = { aid = 0
                  , userid = 1
                  , firstName = "First Name"
                  , lastName = "Last Name" }

-- Task Response Boilerplate (Could define a wrapper sum type) --
genericResponse : Task Http.Error ResultResp -> Cmd Msg
genericResponse task = task
  |> Task.mapError toString
  |> Task.perform Error (\rr -> FromAdminBackend <| AdminResultResp rr)

userDetailResponse : Task Http.Error (Author) -> Cmd Msg
userDetailResponse task = task
  |> Task.mapError toString
  |> Task.perform Error (\user -> FromAdminBackend <| AdminUserDetail user)

postDetailResponse : Task Http.Error (BlogPost) -> Cmd Msg
postDetailResponse task = task
  |> Task.mapError toString
  |> Task.perform Error (\post -> FromAdminBackend <| AdminPostDetail post)

seriesDetailResponse : Task Http.Error (BlogSeries) -> Cmd Msg
seriesDetailResponse task = task
  |> Task.mapError toString
  |> Task.perform Error (\series -> FromAdminBackend <| AdminSeriesDetail series)


-- Update specific fields. There must be a better way...
updatePostPublished : Api.BlogPost -> String -> Msg
updatePostPublished post pubdate = case pubdate of
  "" -> FromAdminBackend (AdminPostDetail { post | pubdate = Nothing })
  dateAttempt -> case fromString dateAttempt of
    Err _ -> NoOp
    Ok date -> FromAdminBackend (AdminPostDetail { post | pubdate = Just date })

updatePostTitle : Api.BlogPost -> String -> Msg
updatePostTitle post title = FromAdminBackend (AdminPostDetail { post | title = title })

updatePostBody : Api.BlogPost -> String -> Msg
updatePostBody post body = case body of
  "" -> FromAdminBackend (AdminPostDetail { post | body = Nothing })
  _ -> FromAdminBackend (AdminPostDetail { post | body = Just body })

updatePostSynopsis : Api.BlogPost -> String -> Msg
updatePostSynopsis post synopsis = case synopsis of
  "" -> FromAdminBackend (AdminPostDetail { post | synopsis = Nothing })
  _ -> FromAdminBackend (AdminPostDetail { post | synopsis = Just synopsis })

updateSeriesId : Api.BlogPost -> String -> Msg
updateSeriesId post sid = case sid of
  "" -> FromAdminBackend (AdminPostDetail { post | seriesId = Nothing })
  someStr -> case toInt someStr of
    Err _ -> NoOp
    Ok seriesId -> FromAdminBackend (AdminPostDetail { post | seriesId = Just seriesId })

updateOrdering : Api.BlogPost -> String -> Msg
updateOrdering post order = case order of
  "" -> FromAdminBackend (AdminPostDetail { post | seriesId = Nothing })
  someOrder -> case toInt someOrder of
    Err _ -> NoOp
    Ok ordering -> FromAdminBackend (AdminPostDetail { post | ordinal = Just ordering })

updateSeriesName : Api.BlogSeries -> String -> Msg
updateSeriesName series name = FromAdminBackend (AdminSeriesDetail { series | name = name })

updateSeriesDescription : Api.BlogSeries -> String -> Msg
updateSeriesDescription series description = FromAdminBackend (AdminSeriesDetail { series | description = description })
