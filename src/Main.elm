module Main exposing (..)

import Dom
import Html exposing (Html, Attribute, small, div, ul, label, span, li, a, input, button, text)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput, onClick, onBlur, onDoubleClick, keyCode)
import Html.Keyed as Keyed
import Tuple exposing (first, second)
import Json.Decode as Json
import Task

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Task =
    { id : Int
    , description : String
    , isComplete : Bool
    , isEditing : Bool
    }


type alias Model =
    { uid : Int
    , description : String
    , tasks : List Task
    , visibilityFilter : String
    , checkAll : Bool
    }

init : ( Model, Cmd Msg )
init =
     model ! []

model : Model
model =
    Model 3 "" initStartTasks "all" False


initStartTasks : List Task
initStartTasks =
    List.map mapTask startingTasks


startingTasks : List ( Int, String )
startingTasks =
    [ ( 0, "Buy milk" )
    , ( 1, "Read paper" )
    , ( 2, "Post letter" )
    ]


mapTask : ( Int, String ) -> Task
mapTask obj =
    taskInit (first obj) (second obj)


taskInit : Int -> String -> Task
taskInit id description =
    { id = id
    , description = description
    , isComplete = False
    , isEditing = False
    }



-- UPDATE


type Msg
    = NoOp
    | DescriptionChange String
    | Add
    | EditingTask Int Bool
    | UpdateTask Int String
    | ApplyFilter String
    | ToggleStatus Int Bool
    | ToggleAll
    | DeleteOne Int
    | DeleteComplete


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
          model ! []

        DescriptionChange newContent ->
            { model | description = newContent }
            ! []

        Add ->
            { model
                | uid = model.uid + 1
                , description = ""
                , tasks =
                    if String.isEmpty model.description then
                        model.tasks
                    else
                        model.tasks ++ [ taskInit model.uid model.description ]
                , checkAll = False
            }
            ! []

        EditingTask id isEditing ->
          let
            setTaskEditState t =
              if t.id == id then
                { t | isEditing = isEditing }
              else
                t

            focus =
              Dom.focus ("task-input-" ++ toString id)
          in
            { model | tasks = List.map setTaskEditState model.tasks }
            ! [ Task.attempt (\_ -> NoOp) focus ]

        UpdateTask id desc ->
          let
            updateDesc t =
              if t.id == id then
                { t | description = desc }
              else
                t
          in
            { model | tasks = List.map updateDesc model.tasks }
            ! []

        ApplyFilter filter ->
            { model | visibilityFilter = filter }
            ! []

        ToggleStatus id isCompleted ->
            let
                updateEntry t =
                    if t.id == id then
                        { t | isComplete = isCompleted }
                    else
                        t
            in
                { model | tasks = List.map updateEntry model.tasks }
                ! []

        ToggleAll ->
            { model
                | checkAll = not model.checkAll
                , tasks = List.map (\t -> { t | isComplete = not model.checkAll }) model.tasks
            }
            ! []

        DeleteOne id ->
            { model | tasks = List.filter (\t -> t.id /= id) model.tasks }
            ! []

        DeleteComplete ->
            { model | tasks = List.filter (\t -> not t.isComplete) model.tasks }
            ! []

-- VIEW


view : Model -> Html Msg
view model =
    let
        hasCompleted =
            List.any (\t -> t.isComplete) model.tasks

        tasksLeft =
            List.length (List.filter (\t -> not t.isComplete) model.tasks)

        tasksComplete =
            (List.length model.tasks) - tasksLeft
    in
        div [ id "task-app", class "center-contents flex-column" ]
            [ div [ class "flex-row" ]
                [ viewTickbox model.checkAll [ onClick ToggleAll ]
                , viewInputBox model
                ]
            , div [] [ viewTasks model ]
            , div [ class "spaced-contents" ]
                [ small [ id "task-count" ] [ text ((toString tasksLeft) ++ " left") ]
                , viewFilters model.visibilityFilter
                , button [ type_ "button", class "button-link", classList [ ( "hidden", not hasCompleted ) ], onClick DeleteComplete ] [ text ("clear completed (" ++ (toString tasksComplete) ++ ")") ]
                ]
            ]


viewInputBox : Model -> Html Msg
viewInputBox model =
    div [ class "has-float-label" ]
        [ input [ type_ "text", placeholder " ", value model.description, onInput DescriptionChange, onEnter Add ] []
        , label [] [ text "What do you need to do?" ]
        ]


viewTasks : Model -> Html Msg
viewTasks model =
    let
        keyedLiItems =
          List.map (\t -> ((toString t.id), viewTaskItem t)) (List.filter (taskFilter model.visibilityFilter) model.tasks)
    in
        Keyed.ul [ id "task-list" ]
                 keyedLiItems


taskFilter : String -> Task -> Bool
taskFilter filter =
    filterTasks filter


filterTasks : String -> Task -> Bool
filterTasks filter task =
    case filter of
        "active" ->
            not task.isComplete

        "complete" ->
            task.isComplete

        _ ->
            True


viewTaskItem : Task -> Html Msg
viewTaskItem task =
    li [ class "list-item", classList [("editing", task.isEditing)], id ("task-" ++ toString task.id) ]
        [ viewTickbox task.isComplete [ onClick (ToggleStatus task.id (not task.isComplete)) ]
        , span [ class "task-text", onDoubleClick (EditingTask task.id True) ] [ text task.description ]
        , input [ type_ "text"
                , id ("task-input-" ++ toString task.id)
                , class "task-edit-input"
                , value task.description
                , onEnter (EditingTask task.id False)
                , onBlur (EditingTask task.id False)
                , onInput (UpdateTask task.id)
                ] []
        , button [ type_ "button", class "button-icon rounded primary", style [ ( "margin-left", "auto" ) ], onClick (DeleteOne task.id) ] [ text "X" ]
        ]


viewTickbox : Bool -> List (Attribute Msg) -> Html Msg
viewTickbox ticked attrs =
    label [ class "tickbox" ]
        [ input ([ type_ "checkbox", checked ticked ] ++ attrs) []
        ]


viewFilters : String -> Html Msg
viewFilters filter =
    div [ class "filters" ]
        [ visibilityFilter "all" filter
        , text " "
        , visibilityFilter "active" filter
        , text " "
        , visibilityFilter "complete" filter
        ]


visibilityFilter : String -> String -> Html Msg
visibilityFilter filter activeFilter =
    a [ href ("#/" ++ filter), classList [ ( "selected", filter == activeFilter ) ], onClick (ApplyFilter filter) ]
        [ text (capitalise filter) ]


capitalise : String -> String
capitalise str =
    (String.left 1 str
        |> String.toUpper
    )
        ++ String.slice 1 (String.length str) str


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
