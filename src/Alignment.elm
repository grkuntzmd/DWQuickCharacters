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
import Html.Attributes exposing (attribute, checked, class, for, id, name, title, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (Decoder, andThen, bool, fail, string, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)


type Alignment
    = Defended
    | Inspired
    | Other
    | Worthy


type alias Model =
    { alignment : Alignment
    , fulfilled : Bool
    , other : String
    }


type Msg
    = Alignment Alignment
    | Fulfilled Bool
    | OtherAlignment String


initialModel : Model
initialModel =
    { alignment = Worthy
    , fulfilled = False
    , other = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Alignment alignment ->
            { model | alignment = alignment } ! []

        Fulfilled value ->
            { model | fulfilled = value } ! []

        OtherAlignment value ->
            { model | other = value } ! []


view : Model -> Html Msg
view model =
    div [ class "border border-primary mt-1 p-2 rounded" ]
        [ h3 [] [ text "Alignment" ]
        , form []
            [ div [ class "form-check mt-1" ]
                [ input
                    [ checked (model.alignment == Worthy)
                    , class "form-check-input"
                    , id "worthy"
                    , name "equipment"
                    , onClick (Alignment Worthy)
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
            , div [ class "form-check mt-1" ]
                [ input
                    [ checked (model.alignment == Defended)
                    , class "form-check-input"
                    , id "defended"
                    , name "equipment"
                    , onClick (Alignment Defended)
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
            , div [ class "form-check mt-1" ]
                [ input
                    [ checked (model.alignment == Inspired)
                    , class "form-check-input"
                    , id "inspired"
                    , name "equipment"
                    , onClick (Alignment Inspired)
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
            , div [ class "form-check mt-1" ]
                [ input
                    [ checked (model.alignment == Other)
                    , class "form-check-input"
                    , id "other"
                    , name "equipment"
                    , onClick (Alignment Other)
                    , type_ "radio"
                    , value "other"
                    ]
                    []
                , div []
                    [ input
                        [ class "form-control w-100"
                        , onInput OtherAlignment
                        , type_ "text"
                        , value model.other
                        ]
                        []
                    ]
                ]
            , div [ class "form-group mt-2" ]
                [ div [ class "form-check" ]
                    [ input
                        [ attribute "data-toggle" "tooltip"
                        , attribute "data-placement" "bottom"
                        , checked model.fulfilled
                        , class "form-check-input"
                        , id "fulfilled"
                        , onCheck Fulfilled
                        , title "I have met my alignment since my last rest."
                        , type_ "checkbox"
                        ]
                        []
                    , label
                        [ attribute "data-toggle" "tooltip"
                        , attribute "data-placement" "bottom"
                        , class "form-check-label"
                        , for "fulfilled"
                        , title "I have met my alignment since my last rest."
                        ]
                        [ text "Alignment Fulfilled Since Last Rest" ]
                    ]
                ]
            ]
        ]


decoder : Decoder Model
decoder =
    Pipeline.decode Model
        |> required "alignment" decoderAlignment
        |> required "fulfilled" bool
        |> required "other" string


decoderAlignment : Decoder Alignment
decoderAlignment =
    string
        |> andThen
            (\str ->
                case str of
                    "defended" ->
                        succeed Defended

                    "inspired" ->
                        succeed Inspired

                    "other" ->
                        succeed Other

                    "worthy" ->
                        succeed Worthy

                    _ ->
                        fail <| "unknown alignment"
            )


encode : Model -> Value
encode model =
    Encode.object
        [ ( "alignment", encodeAlignment model.alignment )
        , ( "fulfilled", Encode.bool model.fulfilled )
        , ( "other", Encode.string model.other )
        ]


encodeAlignment : Alignment -> Value
encodeAlignment alignment =
    case alignment of
        Defended ->
            Encode.string "defended"

        Inspired ->
            Encode.string "inspired"

        Other ->
            Encode.string "other"

        Worthy ->
            Encode.string "worthy"
