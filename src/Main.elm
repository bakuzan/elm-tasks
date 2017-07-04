module Main exposing (..)

import Html exposing (Html, Attribute, text, div, ul, label, input)
import Html.Attributes exposing (id, type_, name, for, value, class, placeholder)
import Html.Events exposing (on, onInput, keyCode)
import Json.Decode as Json


---- MODEL ----

type alias Task =
    { id: Int
    , description: String
    , isComplete: Bool
    }

type alias Model =
    { uid: Int
    , description: String
    , taskList: List Task
    }

emptyState : Model
emptyState =
    { uid = 0
    , description = ""
    , taskList = []
    }

newTask : String -> Int -> Task
newTask desc id =
    { id = id
    , description = desc
    , isComplete = False
    }


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateField
    | CompleteTask
    | Add


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Add ->
        { model
            | uid = model.uid + 1
            , field = ""
            , taskList =
                if String.isEmpty model.description then
                    model.taskList
                else
                    model.taskList ++ [ newTask model.description model.uid ]
        }
            ! []

    UpdateField str ->
        { model | description = str }
            ! []

    CompleteTask ->
      ( model, Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "elm-practice" ]
        [ viewInput model.description
        , div [ class "list-container" ]
          [ ul [ id "todo-list" ]
            []
          ]
        ]

viewInput : String -> Html Msg
viewInput desc =
    div []
        [ input
          [ type_ "text"
          , placeholder "What needs to be done?"
          , name "description"
          , id "description"
          , value desc
          , onInput UpdateField
          , onEnter Add
          ] []
        , label [ for "description" ] [ text "enter a description" ]
        ]

onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg
            else
                Json.fail "not ENTER"
    in
        on "keydown" (Json.andThen isEnter keyCode)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
