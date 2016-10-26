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
              [ text <| "View Site" ]
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

adminPostsTable : List (Api.PostOverview) -> Html Msg
adminPostsTable posts =
  table [] [
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
viewPostInTable : Api.PostOverview -> Html Msg
viewPostInTable post =
  tr [ class "admin-editable"
      , onClick (FromAdminFrontend
                  <| AdminGetDetail
                  <| DetailPost post.pid) ] [
    th [] [ text (toString post.pid) ]
    , th [] [ text post.ptitle ]
    , th [] [ text (BlogViews.fromJustDate post.ppubdate) ]
    , th [] [ text (toString post.pordinal) ]
    , th [] [ text (toString post.pseriesid) ]
    , th [] [ text (BlogViews.fromJustStr post.pseriesname) ]
  ]

adminSeriesTable : List (Api.BlogSeries) -> Html Msg
adminSeriesTable series =
  table [] [
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
  table [] [
    thead [] [
      tr [] [
        th [] [ text "Id" ]
        , th [] [ text "First Name" ]
        , th [] [ text "Last Name" ]
        , th [] [ text "Email" ]
      ]
    ]
    , tbody [] <| List.map viewAuthorInTable authors
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
    , th [] [ text author.email ]
  ]

adminPostEdit : Api.BlogPost -> Html Msg
adminPostEdit post =
  div [ class "edit-main" ] [
    div [ class "row" ] [
      div [ class "eight columns" ] [
          p [] [ b [] [ text "Id: " ], text (toString post.bid) ]
          , p [] [ ]
          , p [] [ text "title" ]
          , p [] [ text post.title ]
          , p [] [ text "pubdate" ]
          , p [] [ text (BlogViews.fromJustDate post.pubdate) ]
          , p [] [ text "Order" ]
          , p [] [ text (toString post.ordinal) ]
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

updatePostBody : Api.BlogPost -> String -> Msg
updatePostBody post body = case body of
  "" -> FromAdminBackend (AdminPostDetail { post | body = Nothing })
  _ -> FromAdminBackend (AdminPostDetail { post | body = Just body })

updatePostSynopsis : Api.BlogPost -> String -> Msg
updatePostSynopsis post synopsis = case synopsis of
  "" -> FromAdminBackend (AdminPostDetail { post | synopsis = Nothing })
  _ -> FromAdminBackend (AdminPostDetail { post | synopsis = Just synopsis })

adminSeriesEdit : Api.BlogSeries -> Html Msg
adminSeriesEdit series =
  div [ class "edit-main" ] [ text "edit series" ]

adminAuthorEdit : Api.Author -> Html Msg
adminAuthorEdit author =
  div [ class "edit-main" ] [ text "edit author" ]
