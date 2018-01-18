module Scores
    exposing
        ( Model
        , Msg(..)
        , Rolls
        , UpMsg(..)
        , initialModel
        , update
        , view
        )

import Html exposing (Html, button, div, form, i, input, label, p, text)
import Html.Attributes as Attributes
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
import Html.Events exposing (onCheck, onClick)
import List.Extra exposing ((!!))
import Maybe.Extra as ME
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


type alias Rolls =
    { str : Maybe Int
    , dex : Maybe Int
    , con : Maybe Int
    , int : Maybe Int
    , wis : Maybe Int
    , cha : Maybe Int
    }


type alias Score =
    { mod : String
    , numeric : Maybe Int
    , text : String
    }


type UpMsg
    = CharismaUp Int
    | ConstitutionUp Int
    | WisdomUp Int


initialModel : Seed -> ( Model, Rolls )
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
        ( { cha = score roll.cha
          , con = score roll.con
          , dex = score roll.dex
          , int = score roll.int
          , locked = False
          , seed = seed_
          , str = score roll.str
          , wis = score roll.wis
          }
        , roll
        )


update : Msg -> Model -> ( Model, Cmd Msg, List UpMsg )
update msg model =
    case msg of
        ChaUp ->
            let
                wis =
                    score model.cha.numeric

                cha =
                    score model.wis.numeric
            in
                ( { model | wis = wis, cha = cha }
                , Cmd.none
                , ME.toList (Maybe.map CharismaUp cha.numeric)
                    ++ ME.toList (Maybe.map WisdomUp wis.numeric)
                )

        ConDown ->
            let
                con =
                    score model.int.numeric
            in
                ( { model | con = con, int = score model.con.numeric }
                , Cmd.none
                , ME.toList (Maybe.map ConstitutionUp con.numeric)
                )

        ConUp ->
            let
                con =
                    score model.dex.numeric
            in
                ( { model | con = con, dex = score model.con.numeric }
                , Cmd.none
                , ME.toList (Maybe.map ConstitutionUp con.numeric)
                )

        DexDown ->
            let
                con =
                    score model.dex.numeric
            in
                ( { model | con = con, dex = score model.con.numeric }
                , Cmd.none
                , ME.toList (Maybe.map ConstitutionUp con.numeric)
                )

        DexUp ->
            ( { model
                | dex = score model.str.numeric
                , str = score model.dex.numeric
              }
            , Cmd.none
            , []
            )

        IntDown ->
            let
                wis =
                    score model.int.numeric
            in
                ( { model | int = score model.wis.numeric, wis = wis }
                , Cmd.none
                , ME.toList (Maybe.map WisdomUp wis.numeric)
                )

        IntUp ->
            let
                con =
                    score model.int.numeric
            in
                ( { model | con = con, int = score model.con.numeric }
                , Cmd.none
                , ME.toList (Maybe.map ConstitutionUp con.numeric)
                )

        Locked value ->
            ( { model | locked = value }, Cmd.none, [] )

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
                ( { model
                    | cha = score roll.cha
                    , con = score roll.con
                    , dex = score roll.dex
                    , int = score roll.int
                    , seed = seed
                    , str = score roll.str
                    , wis = score roll.wis
                  }
                , Cmd.none
                , ME.toList (Maybe.map CharismaUp roll.cha)
                    ++ ME.toList (Maybe.map ConstitutionUp roll.con)
                    ++ ME.toList (Maybe.map WisdomUp roll.wis)
                )

        StrDown ->
            ( { model
                | dex = score model.str.numeric
                , str = score model.dex.numeric
              }
            , Cmd.none
            , []
            )

        WisDown ->
            let
                wis =
                    score model.cha.numeric

                cha =
                    score model.wis.numeric
            in
                ( { model | wis = wis, cha = cha }
                , Cmd.none
                , ME.toList (Maybe.map CharismaUp cha.numeric)
                    ++ ME.toList (Maybe.map WisdomUp wis.numeric)
                )

        WisUp ->
            let
                wis =
                    score model.int.numeric
            in
                ( { model | int = score model.wis.numeric, wis = wis }
                , Cmd.none
                , ME.toList (Maybe.map WisdomUp wis.numeric)
                )


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
                            [ i [ class "fas fa-arrow-down" ] []
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
                            [ i [ class "fas fa-arrow-up" ] []
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
