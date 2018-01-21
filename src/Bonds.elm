module Bonds exposing (Model, Msg(..), initialModel, update, view)

import Dom
import Html exposing (Html, div, form, h3, text, textarea)
import Html.Attributes exposing (class, id, style, value)
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
    let
        visible =
            if model.editing then
                textarea
                    [ class "form-control"
                    , id "bonds"
                    , onBlur (Editing False)
                    , onInput Bonds
                    , style [ ( "flex", "1" ) ]
                    , value model.bonds
                    ]
                    []
            else
                Markdown.toHtml
                    [ style [ ( "flex", "1" ) ] ]
                    (if String.isEmpty model.bonds then
                        "[Markdown](https://daringfireball.net/projects/markdown/syntax) enabled"
                     else
                        model.bonds
                    )
    in
        div
            [ class "align-items-stretch border border-primary d-flex flex-column justify-content-between mt-1 p-2 rounded"
            , style [ ( "flex", "1" ) ]
            ]
            [ h3 [] [ text "Bonds" ]
            , form
                [ class "align-items-stretch border border-primary d-flex flex-column justify-content-between p-1 rounded w-100"
                , onClick (Editing True)
                , style [ ( "flex", "1" ) ]
                ]
                [ visible
                ]
            ]
