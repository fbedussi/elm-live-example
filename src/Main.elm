module Main exposing (main)

import Browser
import Html exposing (Html, button, div, form, input, label, li, span, text, ul)
import Html.Attributes exposing (class, for, id, type_, value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { todos : List Todo
    , newTodoText : String
    }


type alias Todo =
    { text : String
    , done : Bool
    , id : Id
    }


type Msg
    = UpdateNewTodoText String
    | AddTodo
    | ToggleTodoDone Id


type Id
    = Id Int


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    { todos = []
    , newTodoText = ""
    }


view : Model -> Html Msg
view model =
    div
        []
        [ form
            []
            [ label
                [ for "new-todo-text" ]
                []
            , input
                [ type_ "text", id "new-todo-text", onInput UpdateNewTodoText, value model.newTodoText ]
                []
            , button
                [ type_ "button", onClick AddTodo ]
                [ text "add" ]
            ]
        , ul
            []
            (model.todos |> List.map renderTodo)
        ]


renderTodo : Todo -> Html Msg
renderTodo todo =
    li
        []
        [ span
            [ class
                (if todo.done then
                    "done"

                 else
                    ""
                )
            ]
            [ text todo.text ]
        , button
            [ onClick (ToggleTodoDone todo.id) ]
            [ text
                (if todo.done then
                    "undo"

                 else
                    "done"
                )
            ]
        ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateNewTodoText text ->
            { model | newTodoText = text }

        AddTodo ->
            { model | todos = createNewTodo model.newTodoText model.todos :: model.todos, newTodoText = "" }

        ToggleTodoDone id ->
            { model | todos = toggleTodoDone model.todos id }


createNewTodo : String -> List Todo -> Todo
createNewTodo text todos =
    { text = text
    , id = getNextId todos
    , done = False
    }


getNextId : List Todo -> Id
getNextId todos =
    let
        lastId =
            todos
                |> List.map .id
                |> List.map (\(Id id) -> id)
                |> List.maximum
    in
    case lastId of
        Just id ->
            Id (id + 1)

        Nothing ->
            Id 0


toggleTodoDone : List Todo -> Id -> List Todo
toggleTodoDone todos id =
    todos
        |> List.map
            (\todo ->
                if todo.id == id then
                    { todo | done = not todo.done }

                else
                    todo
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
