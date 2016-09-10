module Page exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Page = Home
          | PostIndex
          | PostList
          | PostDetail
          | AboutPage
          | ProjectsPage

pageSkeleton : Html Page
pageSkeleton = div [class "container" ] [
                div [class "row" ] [
                  div [class "three columns", style [("margin-top", "2%")]] [
                    a [onClick Home, class "button", style [("border", "none")]] [
                      h5 [class "u-pull-right"] [text "EKADANTA"]
                    ]
                  ]
                , div [class "nine columns", style [("margin-top", "2%")]] [
                    div [class "top-nav"] [
                      div [class "two columns"] [
                        a [onClick PostList, class "button", style [("border", "none")]] [text "Blog"]
                        ]
                    , div [class "one column"] [text ""]
                    , div [class "two columns"] [
                        a [onClick AboutPage, class "button", style [("border", "none")]] [text "About"]
                      ]
                      , div [class "one column"] [text ""]
                      , div [class "two columns"] [
                        a [onClick ProjectsPage, class "button", style [("border", "none")]] [text "Projects"]
                      ]
                    ]
                  ]
                ]
              ]
