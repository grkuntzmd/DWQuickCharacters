module Scores exposing (Model, Msg(..), initialModel, update, view)

import Html exposing (Html, button, div, form, i, input, label, p, text)
import Html.Attributes
    exposing
        ( class
        , classList
        , for
        , hidden
        , id
        , readonly
        , type_
        , value
        )
import Html.Attributes as Attributes
import Html.Attributes.Aria exposing (ariaHidden)
import Html.Events exposing (onCheck, onClick)
import List.Extra exposing ((!!))
import Random.Pcg exposing (Seed, int, list, step)


type alias Model =
    { cha : Score
    , con : Score
    , dex : Score
    , int : Score
    , locked : Bool
    , seed : Seed
    , str : Score
    , wis : Score
    }


type Msg
    = ChaUp
    | ConDown
    | ConUp
    | DexDown
    | DexUp
    | IntDown
    | IntUp
    | Locked Bool
    | Reroll
    | StrDown
    | WisDown
    | WisUp


type alias Score =
    { mod : String
    , numeric : Maybe Int
    , text : String
    }


initialModel : Seed -> Model
initialModel seed =
    let
        ( scores, seed_ ) =
            rollScores seed

        roll =
            { str = scores !! 0
            , dex = scores !! 1
            , con = scores !! 2
            , int = scores !! 3
            , wis = scores !! 4
            , cha = scores !! 5
            }
    in
        { cha = score roll.cha
        , con = score roll.con
        , dex = score roll.dex
        , int = score roll.int
        , locked = False
        , seed = seed_
        , str = score roll.str
        , wis = score roll.wis
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChaUp ->
            { model
                | wis = score model.cha.numeric
                , cha = score model.wis.numeric
            }
                ! []

        ConDown ->
            { model
                | con = score model.int.numeric
                , int = score model.con.numeric
            }
                ! []

        ConUp ->
            { model
                | con = score model.dex.numeric
                , dex = score model.con.numeric
            }
                ! []

        DexDown ->
            { model
                | con = score model.dex.numeric
                , dex = score model.con.numeric
            }
                ! []

        DexUp ->
            { model
                | dex = score model.str.numeric
                , str = score model.dex.numeric
            }
                ! []

        IntDown ->
            { model
                | int = score model.wis.numeric
                , wis = score model.int.numeric
            }
                ! []

        IntUp ->
            { model
                | con = score model.int.numeric
                , int = score model.con.numeric
            }
                ! []

        Locked value ->
            { model | locked = value } ! []

        Reroll ->
            let
                ( scores, seed ) =
                    rollScores model.seed

                roll =
                    { str = scores !! 0
                    , dex = scores !! 1
                    , con = scores !! 2
                    , int = scores !! 3
                    , wis = scores !! 4
                    , cha = scores !! 5
                    }
            in
                { model
                    | cha = score roll.cha
                    , con = score roll.con
                    , dex = score roll.dex
                    , int = score roll.int
                    , seed = seed
                    , str = score roll.str
                    , wis = score roll.wis
                }
                    ! []

        StrDown ->
            { model
                | dex = score model.str.numeric
                , str = score model.dex.numeric
            }
                ! []

        WisDown ->
            { model
                | wis = score model.cha.numeric
                , cha = score model.wis.numeric
            }
                ! []

        WisUp ->
            { model
                | int = score model.wis.numeric
                , wis = score model.int.numeric
            }
                ! []


view : Model -> Html Msg
view model =
    let
        ( hidden_, readonly_, typ ) =
            if not model.locked then
                ( False, True, "text" )
            else
                ( True, False, "number" )

        scoreRow : String -> String -> Maybe Msg -> Maybe Msg -> String -> String -> Html Msg
        scoreRow ability modifier downMsg upMsg scoreField modField =
            div [ class "form-group row" ]
                [ label [ class "col-lg-3 col-md-4", for ability ] [ text ability ]
                , div
                    [ classList
                        [ ( "col-lg-3 col-md-2", not hidden_ )
                        , ( "col-lg-5 col-md-4", hidden_ )
                        ]
                    ]
                    [ input
                        [ class "form-control text-right w-100"
                        , id ability
                        , Attributes.max "18"
                        , Attributes.min "1"
                        , readonly readonly_
                        , type_ typ
                        , value scoreField
                        ]
                        []
                    ]
                , case downMsg of
                    Just msg ->
                        button
                            [ class "btn btn-outline-primary col-1"
                            , hidden hidden_
                            , onClick msg
                            , type_ "button"
                            ]
                            [ i
                                [ ariaHidden True
                                , class "fa fa-arrow-down"
                                ]
                                []
                            ]

                    Nothing ->
                        div [ class "col-1", hidden hidden_ ] []
                , case upMsg of
                    Just msg ->
                        button
                            [ class "btn btn-outline-primary col-1"
                            , hidden hidden_
                            , onClick msg
                            , type_ "button"
                            ]
                            [ i
                                [ ariaHidden True
                                , class "fa fa-arrow-up"
                                ]
                                []
                            ]

                    Nothing ->
                        div [ class "col-1", hidden hidden_ ] []
                , label [ class "col-2", for modifier ] [ text modifier ]
                , div [ class "col-2" ]
                    [ input
                        [ class "form-control text-right w-100"
                        , id modifier
                        , readonly True
                        , type_ "text"
                        , value modField
                        ]
                        []
                    ]
                ]
    in
        form [ class "border border-primary col-md-6 col-xl-4 mt-1 p-2 rounded" ]
            [ scoreRow "Strength" "STR" (Just StrDown) Nothing model.str.text model.str.mod
            , scoreRow "Dexterity" "DEX" (Just DexDown) (Just DexUp) model.dex.text model.dex.mod
            , scoreRow "Constitution" "CON" (Just ConDown) (Just ConUp) model.con.text model.con.mod
            , scoreRow "Intelligence" "INT" (Just IntDown) (Just IntUp) model.int.text model.int.mod
            , scoreRow "Wisdom" "WIS" (Just WisDown) (Just WisUp) model.wis.text model.wis.mod
            , scoreRow "Charisma" "CHA" Nothing (Just ChaUp) model.cha.text model.cha.mod
            , div [ class "form-group row" ]
                [ div [ class "col-5" ]
                    [ div [ class "form-check" ]
                        [ input
                            [ class "form-check-input"
                            , id "locked"
                            , onCheck Locked
                            , type_ "checkbox"
                            ]
                            []
                        , label
                            [ class "form-check-label"
                            , for "locked"
                            ]
                            [ text "Lock Scores" ]
                        ]
                    ]
                , div
                    [ class "col-7" ]
                    [ button
                        [ class "btn btn-primary float-right"
                        , hidden hidden_
                        , onClick Reroll
                        , type_ "button"
                        ]
                        [ text "Re-roll" ]
                    ]
                ]
            ]


modifiers : Maybe Int -> String
modifiers =
    Maybe.map
        (\s ->
            if s <= 3 then
                "-3"
            else if s <= 5 then
                "-2"
            else if s <= 8 then
                "-1"
            else if s <= 12 then
                "0"
            else if s <= 15 then
                "+1"
            else if s <= 17 then
                "+2"
            else
                "+3"
        )
        >> Maybe.withDefault ""


rollScores : Seed -> ( List Int, Seed )
rollScores seed =
    let
        ( rolls, seed_ ) =
            step (list 6 (list 4 (int 1 6))) seed
    in
        ( List.map
            (\dice ->
                List.sum dice - (List.minimum dice |> Maybe.withDefault 0)
            )
            rolls
            |> List.sort
            |> List.reverse
        , seed_
        )


score : Maybe Int -> Score
score value =
    { mod = modifiers value
    , numeric = value
    , text = Maybe.map toString value |> Maybe.withDefault ""
    }
