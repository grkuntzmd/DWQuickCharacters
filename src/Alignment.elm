module Alignment exposing (Model, Msg(..), initialModel, update, view)

import Html exposing (Html, div, form, h2, input, label, text)
import Html.Attributes exposing (checked, class, for, id, name, type_, value)
import Html.Events exposing (onClick)


type Alignment
    = Defended
    | Inspired
    | Worthy


type alias Model =
    { alignment : Alignment }


type Msg
    = DefendedMsg
    | InspiredMsg
    | WorthyMsg


initialModel : Model
initialModel =
    { alignment = Worthy }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DefendedMsg ->
            { model | alignment = Defended } ! []

        InspiredMsg ->
            { model | alignment = Inspired } ! []

        WorthyMsg ->
            { model | alignment = Worthy } ! []


view : Model -> Html Msg
view model =
    div [ class "border border-primary col-12 mt-1 p-2 rounded" ]
        [ h2 [] [ text "Alignment" ]
        , form []
            [ div [ class "form-check" ]
                [ input
                    [ checked (model.alignment == Worthy)
                    , class "form-check-input"
                    , id "worthy"
                    , name "equipment"
                    , onClick WorthyMsg
                    , type_ "radio"
                    , value "worthy"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "worthy"
                    ]
                    [ text """
                      "I defeated a worthy opponent today."
                    """ ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ checked (model.alignment == Defended)
                    , class "form-check-input"
                    , id "defended"
                    , name "equipment"
                    , onClick DefendedMsg
                    , type_ "radio"
                    , value "defended"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "defended"
                    ]
                    [ text """
                      "I defended someone who couldn’t defend themselves."
                    """ ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ checked (model.alignment == Inspired)
                    , class "form-check-input"
                    , id "inspired"
                    , name "equipment"
                    , onClick InspiredMsg
                    , type_ "radio"
                    , value "inspired"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "inspired"
                    ]
                    [ text """
                      "I inspired my allies to try something very brave, or very stupid."
                    """ ]
                ]
            ]
        ]
