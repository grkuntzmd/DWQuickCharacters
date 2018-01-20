module Bonds exposing (Model, Msg(..), initialModel, update, view)

import Dom
import Html exposing (Html, div, form, h2, text, textarea)
import Html.Attributes exposing (class, hidden, id, rows, style, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Markdown
import Result exposing (Result(..))
import Task


type alias Model =
    { bonds : String
    , editing : Bool
    }


type Msg
    = Bonds String
    | Editing Bool
    | FocusOtherItems (Result Dom.Error ())


initialModel : Model
initialModel =
    { bonds = ""
    , editing = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Bonds value ->
            { model | bonds = value } ! []

        Editing value ->
            let
                cmds =
                    if value then
                        [ Dom.focus "bonds"
                            |> Task.attempt FocusOtherItems
                        ]
                    else
                        []
            in
                { model | editing = value } ! cmds

        FocusOtherItems _ ->
            model ! []


view : Model -> Html Msg
view model =
    div
        [ class "border border-primary col-12 mt-1 p-2 rounded" ]
        [ h2 [] [ text "Bonds" ]
        , form []
            [ div
                [ class "border border-primary h-100 p-1 rounded w-100"
                , onClick (Editing True)
                , style [ ( "min-height", "6rem" ) ]
                ]
                [ Markdown.toHtml
                    [ hidden model.editing ]
                    (if String.isEmpty model.bonds then
                        "[Markdown](https://daringfireball.net/projects/markdown/syntax) enabled"
                     else
                        model.bonds
                    )
                , textarea
                    [ class "form-control h-100 w-100"
                    , hidden <| not model.editing
                    , id "bonds"
                    , onBlur (Editing False)
                    , onInput Bonds
                    , rows 4
                    , value model.bonds
                    ]
                    []
                ]
            ]
        ]
