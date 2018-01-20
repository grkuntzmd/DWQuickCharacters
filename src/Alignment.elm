module Alignment exposing (Model, Msg(..), initialModel, update, view)

import Html exposing (Html, div, form, h2, input, label, text)
import Html.Attributes exposing (class, for, id, name, type_, value)


type alias Model =
    {}


type Msg
    = None


initialModel : Model
initialModel =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            model ! []


view : Model -> Html Msg
view model =
    div [ class "border border-primary col-12 mt-1 p-2 rounded" ]
        [ h2 [] [ text "Alignment" ]
        , form []
            [ div [ class "form-check" ]
                [ input
                    [ class "form-check-input"
                    , id "worthy"
                    , name "equipment"
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
                    [ class "form-check-input"
                    , id "defended"
                    , name "equipment"
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
                    [ class "form-check-input"
                    , id "inspired"
                    , name "equipment"
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
