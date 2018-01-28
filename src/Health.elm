module Health
    exposing
        ( Model
        , Msg(..)
        , decoder
        , encode
        , initialModel
        , update
        , view
        )

import Html exposing (Html, button, div, form, i, input, label, text)
import Html.Attributes as Attributes
    exposing
        ( attribute
        , class
        , for
        , id
        , style
        , title
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, int, maybe, string)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EE
import Random.Pcg as R exposing (Seed, step)


type alias Model =
    { armor : Maybe Int
    , armorText : String
    , currentHP : Maybe Int
    , currentHPText : String
    , maximumHP : Maybe Int
    , maximumHPText : String
    , seed : Seed
    , xp : Maybe Int
    , xpText : String
    }


type Msg
    = Armor String
    | Constitution Int
    | CurrentHP String
    | LevelUp
    | MaximumHP String
    | XP String


initialModel : Seed -> Int -> Model
initialModel seed constitution =
    let
        constitutionText =
            toString constitution
    in
        { armor = Just 0
        , armorText = "0"
        , currentHP = Just constitution
        , currentHPText = constitutionText
        , maximumHP = Just constitution
        , maximumHPText = constitutionText
        , seed = seed
        , xp = Just 0
        , xpText = "0"
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Armor value ->
            let
                armor =
                    String.toInt value |> Result.toMaybe
            in
                { model
                    | armor = armor
                    , armorText =
                        Maybe.map toString armor
                            |> Maybe.withDefault model.armorText
                }
                    ! []

        Constitution value ->
            { model
                | currentHP = Just value
                , currentHPText = toString value
                , maximumHP = Just value
                , maximumHPText = toString value
            }
                ! []

        CurrentHP value ->
            let
                currentHP =
                    String.toInt value |> Result.toMaybe

                ( currentHP_, currentHPText ) =
                    if Maybe.map2 (<=) currentHP model.maximumHP == Just True then
                        ( currentHP
                        , Maybe.map toString currentHP
                            |> Maybe.withDefault model.currentHPText
                        )
                    else
                        ( model.maximumHP, model.maximumHPText )
            in
                { model
                    | currentHP = currentHP_
                    , currentHPText = currentHPText
                }
                    ! []

        LevelUp ->
            let
                ( inc, seed ) =
                    step (R.int 1 6) model.seed

                maximumHP =
                    Maybe.map ((+) inc) model.maximumHP

                xp =
                    Maybe.map (\x -> x - 5 |> max 0) model.xp
            in
                { model
                    | maximumHP = maximumHP
                    , maximumHPText =
                        Maybe.map toString maximumHP
                            |> Maybe.withDefault ""
                    , seed = seed
                    , xp = xp
                    , xpText =
                        Maybe.map toString xp
                            |> Maybe.withDefault model.xpText
                }
                    ! []

        MaximumHP value ->
            let
                maximumHP =
                    String.toInt value |> Result.toMaybe
            in
                { model
                    | maximumHP = maximumHP
                    , maximumHPText =
                        Maybe.map toString maximumHP
                            |> Maybe.withDefault model.maximumHPText
                }
                    ! []

        XP value ->
            let
                xp =
                    String.toInt value |> Result.toMaybe
            in
                { model
                    | xp = xp
                    , xpText =
                        Maybe.map toString xp
                            |> Maybe.withDefault model.xpText
                }
                    ! []


view : Model -> Html Msg
view model =
    form
        [ class "align-items-center border border-primary col-12 d-grid mt-1 p-2 rounded"
        , style
            [ ( "grid", "auto / auto 1fr auto 1fr auto" )
            , ( "grid-gap", "10px 15px" )
            ]
        ]
        [ label [ class "grid-area-auto", for "current-hp" ] [ text "Current HP" ]
        , div [ class "grid-area-auto" ]
            [ input
                [ class "form-control text-right w-100"
                , id "current-hp"
                , Attributes.min "0"
                , onInput CurrentHP
                , type_ "number"
                , value model.currentHPText
                ]
                []
            ]
        , label [ class "grid-area-auto", for "maximum-hp" ] [ text "Maximum HP" ]
        , div [ style [ ( "grid-area", "auto / 4 / auto / 6" ) ] ]
            [ input
                [ class "form-control text-right w-100"
                , id "maximum-hp"
                , Attributes.min "0"
                , onInput MaximumHP
                , type_ "number"
                , value model.maximumHPText
                ]
                []
            ]
        , label [ class "grid-area-auto", for "armor" ] [ text "Armor" ]
        , div [ class "grid-area-auto" ]
            [ input
                [ class "form-control text-right w-100"
                , id "armor"
                , Attributes.min "0"
                , onInput Armor
                , type_ "number"
                , value model.armorText
                ]
                []
            ]
        , label [ class "grid-area-auto", for "xp" ] [ text "XP" ]
        , div [ class "grid-area-auto" ]
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
        , div [ class "grid-area-auto" ]
            [ button
                [ attribute "data-toggle" "tooltip"
                , attribute "data-placement" "bottom"
                , class "btn btn-outline-primary"
                , onClick LevelUp
                , title "Level Up: Subtract 5 from your XP and add 1d6 to your Maximum HP."
                , type_ "button"
                ]
                [ i [ class "fas fa-level-up-alt" ] []
                ]
            ]
        ]


decoder : Seed -> Decoder Model
decoder seed =
    Pipeline.decode Model
        |> optional "armor" (maybe int) Nothing
        |> required "armorText" string
        |> optional "currentHP" (maybe int) Nothing
        |> required "currentHPText" string
        |> optional "maximumHP" (maybe int) Nothing
        |> required "maximumHPText" string
        |> hardcoded seed
        |> optional "xp" (maybe int) Nothing
        |> required "xpText" string


encode : Model -> Value
encode model =
    Encode.object
        [ ( "armor", EE.maybe Encode.int model.armor )
        , ( "armorText", Encode.string model.armorText )
        , ( "currentHP", EE.maybe Encode.int model.currentHP )
        , ( "currentHPText", Encode.string model.currentHPText )
        , ( "maximumHP", EE.maybe Encode.int model.maximumHP )
        , ( "maximumHPText", Encode.string model.maximumHPText )
        , ( "xp", EE.maybe Encode.int model.xp )
        , ( "xpText", Encode.string model.xpText )
        ]
