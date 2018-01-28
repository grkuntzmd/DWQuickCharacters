module Bonds
    exposing
        ( Model
        , Msg(..)
        , decoder
        , encode
        , initialModel
        , update
        , view
        )

import Dom
import Html exposing (Html, div, form, h3, text, textarea)
import Html.Attributes exposing (class, id, value)
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode exposing (Decoder, string)
import Json.Decode.Pipeline as Pipeline exposing (custom, hardcoded)
import Json.Encode as Encode exposing (Value)
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
                    [ class "flex-1 form-control"
                    , id "bonds"
                    , onBlur (Editing False)
                    , onInput Bonds
                    , value model.bonds
                    ]
                    []
            else
                Markdown.toHtml
                    [ class "flex-1" ]
                    (if String.isEmpty model.bonds then
                        "[Markdown](https://daringfireball.net/projects/markdown/syntax) enabled"
                     else
                        model.bonds
                    )
    in
        div
            [ class "align-items-stretch border border-primary d-flex flex-1 flex-column justify-content-between mt-1 p-2 rounded" ]
            [ h3 [] [ text "Bonds" ]
            , form
                [ class "align-items-stretch border border-primary d-flex flex-1 flex-column justify-content-between p-1 rounded w-100"
                , onClick (Editing True)
                ]
                [ visible
                ]
            ]


decoder : Decoder Model
decoder =
    Pipeline.decode Model
        |> custom string
        |> hardcoded False


encode : Model -> Value
encode model =
    Encode.string model.bonds
