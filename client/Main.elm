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


main : Program Never
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }

-- MODEL

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

--
-- init =
--   let
--     model =
--       { error = Nothing
--       , posts = []
--       }
--   in
--     model ! []

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
    --| PostDetail BlogPost

-- type FromUi
--     = AddPostInputChange String
--     | AddPostButton
--     | SelectPost


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
    div []
        <| (List.map viewPost state.posts)
        -- ++ [ input [ onInput (FromUi << AddPostInputChange) ] []
        --    , button [ onClick (FromUi AddPostButton) ] [ text "add post" ]
        --    ]


viewPost : BlogPost -> Html Msg
viewPost post =
    div []
        <| [ text (post.title)
           , text " - "
           , text (toString post.synopsis)
           , text (toString post.published)
           , text (toString post.body)
           , text (toString post.authorId)
           , text (toString post.created)
           , text (toString post.modified)
           , text (toString post.pubdate)
           ]
