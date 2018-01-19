module Scores
    exposing
        ( Model
        , Msg(..)
        , Rolls
        , UpMsg(..)
        , dragged
        , initialModel
        , subscriptions
        , update
        , view
        )

import DnD
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h3
        , h5
        , i
        , input
        , label
        , text
        )
import Html.Attributes as Attributes
    exposing
        ( class
        , classList
        , for
        , hidden
        , id
        , readonly
        , style
        , type_
        , value
        )
import Html.Events exposing (onCheck, onClick)
import Random.Pcg as R exposing (Seed, int, list, step)


type alias Draggable =
    DnD.Draggable Stat ( Stat, Maybe Int )


type alias DraggableMsg =
    DnD.Msg Stat ( Stat, Maybe Int )


type alias Model =
    { cha : Score
    , con : Score
    , dex : Score
    , draggable : Draggable
    , int : Score
    , locked : Bool
    , scores : List Int
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
    | DnDMsg DraggableMsg
    | Dropped Stat ( Stat, Maybe Int )
    | IntDown
    | IntUp
    | Locked Bool
    | Reroll
    | StrDown
    | WisDown
    | WisUp


type alias Rolls =
    { str : Int
    , dex : Int
    , con : Int
    , int : Int
    , wis : Int
    , cha : Int
    }


type alias Score =
    { mod : String
    , numeric : Maybe Int
    , text : String
    }


type Stat
    = Strength
    | Dexterity
    | Constitution
    | Intelligence
    | Wisdom
    | Charisma


type UpMsg
    = CharismaUp Int
    | ConstitutionUp Int
    | WisdomUp Int


initialModel : Seed -> ( Model, Rolls )
initialModel seed =
    let
        ( rolls, seed_ ) =
            rollScores seed
    in
        ( { cha = score <| Just rolls.cha
          , con = score <| Just rolls.con
          , dex = score <| Just rolls.dex
          , draggable = dnd.model
          , int = score <| Just rolls.int
          , locked = False
          , scores = [ rolls.str, rolls.dex, rolls.con, rolls.int, rolls.wis, rolls.cha ]
          , seed = seed_
          , str = score <| Just rolls.str
          , wis = score <| Just rolls.wis
          }
        , rolls
        )


update : Msg -> Model -> ( Model, Cmd Msg, List UpMsg )
update msg model =
    case msg of
        ChaUp ->
            chaUp model

        ConDown ->
            conDown model

        ConUp ->
            conUp model

        DexDown ->
            dexDown model

        DexUp ->
            dexUp model

        DnDMsg msg_ ->
            ( { model | draggable = DnD.update msg_ model.draggable }
            , Cmd.none
            , []
            )

        Dropped to from ->
            dropped to from model

        IntDown ->
            intDown model

        IntUp ->
            intUp model

        Locked value ->
            ( { model | locked = value }, Cmd.none, [] )

        Reroll ->
            reroll model

        StrDown ->
            strDown model

        WisDown ->
            wisDown model

        WisUp ->
            wisUp model


subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.draggable


view : Model -> Html Msg
view model =
    let
        scoreRow : Stat -> String -> String -> Maybe Msg -> Maybe Msg -> Score -> Html Msg
        scoreRow stat ability modifier downMsg upMsg score =
            let
                typ =
                    if not model.locked then
                        "text"
                    else
                        "number"
            in
                div [ class "form-group row" ]
                    [ label [ class "col-lg-3 col-md-4", for ability ]
                        [ text ability ]
                    , div
                        [ classList
                            [ ( "col-lg-3 col-md-2", not model.locked )
                            , ( "col-lg-5 col-md-4", model.locked )
                            ]
                        ]
                        [ dnd.droppable stat
                            [ class "border-thick form-control rounded text-right w-100"
                            , class
                                (if
                                    case DnD.getDropMeta model.draggable of
                                        Just to ->
                                            to == stat

                                        Nothing ->
                                            False
                                 then
                                    "bg-info text-white"
                                 else
                                    ""
                                )
                            , hidden model.locked
                            ]
                            [ dnd.draggable ( stat, score.numeric )
                                []
                                [ h5
                                    [ style [ ( "width", "auto" ) ] ]
                                    [ text score.text ]
                                ]
                            ]
                        , input
                            [ class "form-control text-right w-100"
                            , hidden <| not model.locked
                            , id ability
                            , Attributes.max "18"
                            , Attributes.min "1"
                            , type_ typ
                            , value score.text
                            ]
                            []
                        ]
                    , case downMsg of
                        Just msg ->
                            button
                                [ class "btn btn-outline-primary col-1"
                                , hidden model.locked
                                , onClick msg
                                , type_ "button"
                                ]
                                [ i [ class "fas fa-arrow-down" ] []
                                ]

                        Nothing ->
                            div [ class "col-1", hidden model.locked ] []
                    , case upMsg of
                        Just msg ->
                            button
                                [ class "btn btn-outline-primary col-1"
                                , hidden model.locked
                                , onClick msg
                                , type_ "button"
                                ]
                                [ i [ class "fas fa-arrow-up" ] []
                                ]

                        Nothing ->
                            div [ class "col-1", hidden model.locked ] []
                    , label [ class "col-2", for modifier ] [ text modifier ]
                    , div [ class "col-2" ]
                        [ input
                            [ class "form-control text-right w-100"
                            , id modifier
                            , readonly True
                            , type_ "text"
                            , value score.mod
                            ]
                            []
                        ]
                    ]
    in
        form [ class "border border-primary col-md-6 col-xl-4 mt-1 p-2 rounded" ]
            [ h3 [ hidden model.locked ]
                [ text "Use the arrows or drag and drop to rearrange the scores." ]
            , scoreRow Strength "Strength" "STR" (Just StrDown) Nothing model.str
            , scoreRow Dexterity "Dexterity" "DEX" (Just DexDown) (Just DexUp) model.dex
            , scoreRow Constitution "Constitution" "CON" (Just ConDown) (Just ConUp) model.con
            , scoreRow Intelligence "Intelligence" "INT" (Just IntDown) (Just IntUp) model.int
            , scoreRow Wisdom "Wisdom" "WIS" (Just WisDown) (Just WisUp) model.wis
            , scoreRow Charisma "Charisma" "CHA" Nothing (Just ChaUp) model.cha
            , div [ class "form-group mt-2 row" ]
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
                        , hidden model.locked
                        , onClick Reroll
                        , type_ "button"
                        ]
                        [ text "Re-roll" ]
                    ]
                ]
            ]


chaUp : Model -> ( Model, Cmd Msg, List UpMsg )
chaUp model =
    let
        wis =
            score model.cha.numeric

        cha =
            score model.wis.numeric
    in
        ( { model | wis = wis, cha = cha }
        , Cmd.none
        , [ Maybe.map CharismaUp cha.numeric, Maybe.map WisdomUp wis.numeric ]
            |> List.filterMap identity
        )


conDown : Model -> ( Model, Cmd Msg, List UpMsg )
conDown model =
    let
        con =
            score model.int.numeric
    in
        ( { model | con = con, int = score model.con.numeric }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.numeric ]
            |> List.filterMap identity
        )


conUp : Model -> ( Model, Cmd Msg, List UpMsg )
conUp model =
    let
        con =
            score model.dex.numeric
    in
        ( { model | con = con, dex = score model.con.numeric }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.numeric ]
            |> List.filterMap identity
        )


dexDown : Model -> ( Model, Cmd Msg, List UpMsg )
dexDown model =
    let
        con =
            score model.dex.numeric
    in
        ( { model | con = con, dex = score model.con.numeric }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.numeric ]
            |> List.filterMap identity
        )


dexUp : Model -> ( Model, Cmd Msg, List UpMsg )
dexUp model =
    ( { model
        | dex = score model.str.numeric
        , str = score model.dex.numeric
      }
    , Cmd.none
    , []
    )


dnd : DnD.DraggableInit Stat ( Stat, Maybe Int ) Msg
dnd =
    DnD.init DnDMsg Dropped


dragged : Model -> Html Msg
dragged model =
    let
        box : String -> Html Msg
        box value =
            h3
                [ class "align-middle bg-white border-thick rounded text-center text-info"
                , style
                    [ ( "height", "40px" )
                    , ( "width", "50px" )
                    ]
                ]
                [ text value ]
    in
        DnD.dragged model.draggable
            (box
                << Maybe.withDefault ""
                << Maybe.map toString
                << Tuple.second
            )


dropped : Stat -> ( Stat, Maybe Int ) -> Model -> ( Model, Cmd Msg, List UpMsg )
dropped to from model =
    if to == Tuple.first from then
        ( model, Cmd.none, [] )
    else
        let
            src =
                case Tuple.first from of
                    Strength ->
                        model.str

                    Dexterity ->
                        model.dex

                    Constitution ->
                        model.con

                    Intelligence ->
                        model.int

                    Wisdom ->
                        model.wis

                    Charisma ->
                        model.cha

            dst =
                case to of
                    Strength ->
                        model.str

                    Dexterity ->
                        model.dex

                    Constitution ->
                        model.con

                    Intelligence ->
                        model.int

                    Wisdom ->
                        model.wis

                    Charisma ->
                        model.cha

            transfer :
                Stat
                -> Score
                -> ( Model, List (Maybe UpMsg) )
                -> ( Model, List (Maybe UpMsg) )
            transfer stat value ( model, upMsgs ) =
                case stat of
                    Strength ->
                        ( { model | str = value }, upMsgs )

                    Dexterity ->
                        ( { model | dex = value }, upMsgs )

                    Constitution ->
                        ( { model | con = value }
                        , Maybe.map ConstitutionUp value.numeric :: upMsgs
                        )

                    Intelligence ->
                        ( { model | int = value }, upMsgs )

                    Wisdom ->
                        ( { model | wis = value }
                        , Maybe.map WisdomUp value.numeric :: upMsgs
                        )

                    Charisma ->
                        ( { model | cha = value }
                        , Maybe.map CharismaUp value.numeric :: upMsgs
                        )

            ( model_, upMsgs ) =
                transfer to src ( model, [] )
                    |> transfer (Tuple.first from) dst
        in
            ( model_, Cmd.none, upMsgs |> List.filterMap identity )


intDown : Model -> ( Model, Cmd Msg, List UpMsg )
intDown model =
    let
        wis =
            score model.int.numeric
    in
        ( { model | int = score model.wis.numeric, wis = wis }
        , Cmd.none
        , [ Maybe.map WisdomUp wis.numeric ]
            |> List.filterMap identity
        )


intUp : Model -> ( Model, Cmd Msg, List UpMsg )
intUp model =
    let
        con =
            score model.int.numeric
    in
        ( { model | con = con, int = score model.con.numeric }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.numeric ]
            |> List.filterMap identity
        )


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


reroll : Model -> ( Model, Cmd Msg, List UpMsg )
reroll model =
    let
        ( rolls, seed ) =
            rollScores model.seed
    in
        ( { model
            | cha = score <| Just rolls.cha
            , con = score <| Just rolls.con
            , dex = score <| Just rolls.dex
            , int = score <| Just rolls.int
            , scores = [ rolls.str, rolls.dex, rolls.con, rolls.int, rolls.wis, rolls.cha ]
            , seed = seed
            , str = score <| Just rolls.str
            , wis = score <| Just rolls.wis
          }
        , Cmd.none
        , [ CharismaUp rolls.cha, ConstitutionUp rolls.con, WisdomUp rolls.wis ]
        )


rollScores : Seed -> ( Rolls, Seed )
rollScores seed =
    let
        gen =
            list 4 (int 1 6)
                |> R.map
                    (\d ->
                        List.sum d
                            - (List.minimum d
                                |> Maybe.withDefault 0
                              )
                    )
    in
        step
            (R.map Rolls gen
                |> R.andMap gen
                |> R.andMap gen
                |> R.andMap gen
                |> R.andMap gen
                |> R.andMap gen
            )
            seed


score : Maybe Int -> Score
score value =
    { mod = modifiers value
    , numeric = value
    , text = Maybe.map toString value |> Maybe.withDefault ""
    }


strDown : Model -> ( Model, Cmd Msg, List UpMsg )
strDown model =
    ( { model
        | dex = score model.str.numeric
        , str = score model.dex.numeric
      }
    , Cmd.none
    , []
    )


wisDown : Model -> ( Model, Cmd Msg, List UpMsg )
wisDown model =
    let
        wis =
            score model.cha.numeric

        cha =
            score model.wis.numeric
    in
        ( { model | wis = wis, cha = cha }
        , Cmd.none
        , [ Maybe.map CharismaUp cha.numeric, Maybe.map WisdomUp wis.numeric ]
            |> List.filterMap identity
        )


wisUp : Model -> ( Model, Cmd Msg, List UpMsg )
wisUp model =
    let
        wis =
            score model.int.numeric
    in
        ( { model | int = score model.wis.numeric, wis = wis }
        , Cmd.none
        , [ Maybe.map WisdomUp wis.numeric ]
            |> List.filterMap identity
        )
