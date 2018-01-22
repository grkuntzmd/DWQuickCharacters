module Alignment
    exposing
        ( Model
        , Msg(..)
        , decoder
        , encode
        , initialModel
        , update
        , view
        )

import Html exposing (Html, div, form, h3, input, label, text)
import Html.Attributes exposing (checked, class, for, id, name, type_, value)
import Html.Events exposing (onClick)
import Json.Decode exposing (Decoder, andThen, fail, string, succeed)
import Json.Encode as Encode exposing (Value)


type Model
    = Defended
    | Inspired
    | Worthy


type Msg
    = DefendedMsg
    | InspiredMsg
    | WorthyMsg


initialModel : Model
initialModel =
    Worthy


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        DefendedMsg ->
            Defended ! []

        InspiredMsg ->
            Inspired ! []

        WorthyMsg ->
            Worthy ! []


view : Model -> Html Msg
view model =
    div [ class "border border-primary mt-1 p-2 rounded" ]
        [ h3 [] [ text "Alignment" ]
        , form []
            [ div [ class "form-check" ]
                [ input
                    [ checked (model == Worthy)
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
                    [ checked (model == Defended)
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
                    [ checked (model == Inspired)
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


decoder : Decoder Model
decoder =
    string
        |> andThen
            (\str ->
                case str of
                    "defended" ->
                        succeed Defended

                    "inspired" ->
                        succeed Inspired

                    "worthy" ->
                        succeed Worthy

                    _ ->
                        fail <| "unknown alignment"
            )


encode : Model -> Value
encode model =
    case model of
        Defended ->
            Encode.string "defended"

        Inspired ->
            Encode.string "inspired"

        Worthy ->
            Encode.string "worthy"
