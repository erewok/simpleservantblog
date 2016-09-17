module Page exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Types exposing (..)


pageSkeleton : Html Msg
pageSkeleton = div [class "container" ] [
                div [class "row" ] [
                  div [class "three columns", style [("margin-top", "2%")]] [
                    a [onClick (FromFrontend Home), class "button", style [("border", "none")]] [
                      h5 [class "u-pull-right"] [text "EKADANTA"]
                    ]
                  ]
                , div [class "nine columns", style [("margin-top", "2%")]] [
                    div [class "top-nav"] [
                      div [class "two columns"] [
                        a [onClick (FromFrontend SeePostList), class "button" ] [text "Blog"]
                        ]
                    , div [class "one column"] [text ""]
                    , div [class "two columns"] [
                        a [onClick (FromFrontend SeeAboutPage), class "button" ] [text "About"]
                      ]
                      , div [class "one column"] [text ""]
                      , div [class "two columns"] [
                        a [class "button" ] [text "Projects"]
                      ]
                    ]
                  ]
                ]
              ]
