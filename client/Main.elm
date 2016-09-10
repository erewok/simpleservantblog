module Main exposing (..)

import Date exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (Task, perform)
import Api exposing (..)
import Page exposing (..)


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- MODEL

type Model = PostIndex | PostList | PostDetail | AboutPage | ProjectsPage

type alias Posts =
    { posts : List (BlogPost)
    , error : Maybe String
    }

type alias Post =
  { content : BlogPost
  , error : Maybe String
  }

type alias PostId =
    Int

type alias AuthorId =
    Int


init : ( Posts, Cmd Msg )
init =
    let
        state =
            { posts = [], error = Nothing }
    in
        ( state, retrieve )


-- UPDATE


type Msg
    = NoOp
      | FromServer FromServer
      | DataFetchedAll (List BlogPost)
      | Error String


type FromServer
    = Initial Posts

type FromUi
    = SelectPostIndex
    | SelectPostList
    | SelectPostDetail
    | SelectAboutPage
    | SelectProjectsPage


update : Msg -> Posts -> ( Posts, Cmd Msg )
update message s =
    case message of
      NoOp ->
        s ! []
      DataFetchedAll posts ->
        { s | posts = posts, error = Nothing } ! []
      FromServer fromServer ->
        case fromServer of
          Initial posts ->
            { s | error = Nothing } ! [retrieve]
      Error msg ->
        { s | error = Just msg } ! []



toServer : (a -> FromServer) -> Task Http.Error a -> Cmd Msg
toServer tag task =
    perform (Error << toString) (FromServer << tag) task



-- VIEW

retrieve : Cmd Msg
retrieve = Api.getPost
  |> Task.mapError toString
  |> Task.perform Error DataFetchedAll


view : Posts -> Html Msg
view state =
    div [ class "post-container row" ]
        <| postIndex ++
           (List.map viewPost state.posts) ++
           (List.map viewPost state.posts)

        -- ++ [ input [ onInput (FromUi << AddPostInputChange) ] []
        --    , button [ onClick (FromUi AddPostButton) ] [ text "add post" ]
        --    ]

fromJustStr : Maybe String -> String
fromJustStr someval = case someval of
  Nothing -> ""
  Just t -> t

fromJustDate : Maybe Date -> String
fromJustDate somedate = case somedate of
  Nothing -> ""
  Just date -> toString (day date) ++ " "
              ++ toString (month date) ++ " "
              ++ toString (year date)


viewPost : BlogPost -> Html Msg
viewPost post = case post.published of
  True ->
    div [ class "post-content" ] (postTitleAndSynopsis post)
  False -> text ""

postIndex : List (Html a)
postIndex = [ p [] [ text "This is a series of posts on constructing this blog. All posts are listed below."]
            , div [class "six columns "] [text ""]
            ]

postTitleAndSynopsis : BlogPost -> List (Html a)
postTitleAndSynopsis post = [(postTitle post)] ++
                            [ div [ class "row post-synopsis" ] [
                                p [] [ text (fromJustStr post.synopsis)]]
                            ]

postTitle : BlogPost -> Html a
postTitle post = div [ class "row post-title" ] [
                      h3 [ class "" ] [ text (post.title) ]
                    , span [ class "" ] [ text (fromJustDate post.pubdate)]
                    ]


postBody : BlogPost -> List (Html a)
postBody post = [ div [ class "row post-body", style [("margin-top", "5px;")]] [
                    p [] [text (fromJustStr post.body)] ]
                ]
