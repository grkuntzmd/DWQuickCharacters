module Health exposing (Model, Msg(..), initialModel, update, view)

import Html exposing (Html, button, div, form, i, input, label, p, text)
import Html.Attributes as Attributes exposing (attribute, class, for, id, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Random.Pcg exposing (Seed, int, list, step)


type alias Model =
    { currentHP : Maybe Int
    , currentHPText : String
    , maximumHP : Maybe Int
    , maximumHPText : String
    , seed : Seed
    , xp : Maybe Int
    , xpText : String
    }


type Msg
    = Constitution Int
    | LevelUp
    | XP String


initialModel : Seed -> Maybe Int -> Model
initialModel seed constitution =
    let
        constitutionText =
            Maybe.map toString constitution |> Maybe.withDefault ""
    in
        { currentHP = constitution
        , currentHPText = constitutionText
        , maximumHP = constitution
        , maximumHPText = constitutionText
        , seed = seed
        , xp = Just 0
        , xpText = "0"
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Constitution value ->
            { model
                | currentHP = Just value
                , currentHPText = toString value
                , maximumHP = Just value
                , maximumHPText = toString value
            }
                ! []

        LevelUp ->
            let
                ( inc, seed ) =
                    step (int 1 6) model.seed

                currentHP =
                    Maybe.map ((+) inc) model.currentHP

                _ =
                    Debug.log "model.xp" model.xp

                xp =
                    Maybe.map (\x -> x - 5 |> max 0) model.xp

                _ =
                    Debug.log "xp" xp
            in
                { model
                    | currentHP = currentHP
                    , currentHPText = Maybe.map toString currentHP |> Maybe.withDefault ""
                    , seed = seed
                    , xp = xp
                    , xpText = Maybe.map toString xp |> Maybe.withDefault model.xpText
                }
                    ! []

        XP value ->
            let
                xp =
                    String.toInt value |> Result.toMaybe
            in
                { model
                    | xp = xp
                    , xpText = Maybe.map toString xp |> Maybe.withDefault model.xpText
                }
                    ! []


view : Model -> Html Msg
view model =
    form [ class "border border-primary col-md-6 col-xl-4 mt-1 p-2 rounded" ]
        [ div [ class "row" ]
            [ label [ class "col-md-4 col-xl-2", for "current-hp" ] [ text "Current HP" ]
            , div [ class "col-md-2 col-xl-4" ]
                [ input
                    [ class "form-control text-right w-100"
                    , id "current-hp"
                    , Attributes.min "0"
                    , type_ "number"
                    , value model.currentHPText
                    ]
                    []
                ]
            , label [ class "col-md-4 col-xl-2", for "maximum-hp" ] [ text "Maximum HP" ]
            , div [ class "col-md-2 col-xl-4" ]
                [ input
                    [ class "form-control text-right w-100"
                    , id "maximum-hp"
                    , Attributes.min "0"
                    , type_ "number"
                    , value model.maximumHPText
                    ]
                    []
                ]
            ]
        , div [ class "mt-1 row" ]
            [ label [ class "col-md-3 col-xl-2", for "armor" ] [ text "Armor" ]
            , div [ class "col-md-3 col-xl-4" ]
                [ input
                    [ class "form-control text-right w-100"
                    , id "armor"
                    , Attributes.min "0"
                    , type_ "number"
                    ]
                    []
                ]
            , label [ class "col-md-1 col-xl-2", for "xp" ] [ text "XP" ]
            , div [ class "col-md-3 col-xl-3" ]
                [ input
                    [ class "form-control text-right w-100"
                    , id "xp"
                    , Attributes.min "0"
                    , onInput XP
                    , type_ "number"
                    , value model.xpText
                    ]
                    []
                ]
            , div [ class "col-1" ]
                [ button
                    [ attribute "data-toggle" "tooltip"
                    , attribute "data-placement" "top"
                    , class "btn btn-outline-primary"
                    , onClick LevelUp
                    , title "Level Up: Subtract 5 from your XP and add 1d6 to your HP"
                    , type_ "button"
                    ]
                    [ i [ class "fas fa-level-up-alt" ] []
                    ]
                ]
            ]
        ]
