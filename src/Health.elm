module Health exposing (Model, Msg(..), initialModel, update, view)

import Html exposing (Html, button, div, form, i, input, label, p, text)
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
import Random.Pcg exposing (Seed, int, list, step)


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
            in
                { model
                    | currentHP = currentHP
                    , currentHPText =
                        Maybe.map toString currentHP
                            |> Maybe.withDefault model.currentHPText
                }
                    ! []

        LevelUp ->
            let
                ( inc, seed ) =
                    step (int 1 6) model.seed

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
        [ class "border border-primary col-12 mt-1 p-2 rounded"
        , style
            [ ( "align-items", "center" )
            , ( "display", "grid" )
            , ( "grid", "auto / auto 1fr auto 1fr auto" )
            , ( "grid-gap", "10px 15px" )
            ]
        ]
        [ label
            [ for "current-hp"
            , style [ ( "grid-area", "auto / 1 / auto / 2" ) ]
            ]
            [ text "Current HP" ]
        , div [ style [ ( "grid-area", "auto / 2 / auto / 3" ) ] ]
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
        , label
            [ for "maximum-hp"
            , style [ ( "grid-area", "auto / 3 / auto / 4" ) ]
            ]
            [ text "Maximum HP" ]
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
        , label
            [ for "armor"
            , style [ ( "grid-area", "auto / 1 / auto / 2" ) ]
            ]
            [ text "Armor" ]
        , div [ style [ ( "grid-area", "auto / 2 / auto / 3" ) ] ]
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
        , label
            [ for "xp"
            , style [ ( "grid-area", "auto / 3 / auto / 4" ) ]
            ]
            [ text "XP" ]
        , div [ style [ ( "grid-area", "auto / 4 / auto / 5" ) ] ]
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
        , div [ style [ ( "grid-area", "auto / 5 / auto / 6" ) ] ]
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
