module Scores
    exposing
        ( Model
        , Msg(..)
        , Rolls
        , UpMsg(..)
        , decoder
        , dragged
        , encode
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
        , h4
        , h5
        , i
        , input
        , label
        , text
        )
import Html.Attributes as Attributes
    exposing
        ( attribute
        , class
        , classList
        , disabled
        , for
        , hidden
        , id
        , style
        , title
        , type_
        , value
        )
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode exposing (Decoder, bool, int, maybe, string)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EE
import Random.Pcg as R exposing (Seed, step)


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
    , seed : Seed
    , str : Score
    , wis : Score
    }


type Msg
    = CharismaMsg String
    | ChaUp
    | ConDown
    | ConstitutionMsg String
    | ConUp
    | DexDown
    | DexterityMsg String
    | DexUp
    | DnDMsg DraggableMsg
    | Dropped Stat ( Stat, Maybe Int )
    | IntDown
    | IntelligenceMsg String
    | IntUp
    | Locked Bool
    | Reroll
    | StrDown
    | StrengthMsg String
    | WisDown
    | WisdomMsg String
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
    { error : Bool
    , mod : String
    , number : Maybe Int
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
        ( { cha = maybeScore <| Just rolls.cha
          , con = maybeScore <| Just rolls.con
          , dex = maybeScore <| Just rolls.dex
          , draggable = dnd.model
          , int = maybeScore <| Just rolls.int
          , locked = False
          , seed = seed_
          , str = maybeScore <| Just rolls.str
          , wis = maybeScore <| Just rolls.wis
          }
        , rolls
        )


update : Msg -> Model -> ( Model, Cmd Msg, List UpMsg )
update msg model =
    case msg of
        CharismaMsg value ->
            ( { model | cha = score value }, Cmd.none, [] )

        ChaUp ->
            chaUp model

        ConDown ->
            conDown model

        ConstitutionMsg value ->
            ( { model | con = score value }, Cmd.none, [] )

        ConUp ->
            conUp model

        DexDown ->
            dexDown model

        DexterityMsg value ->
            ( { model | dex = score value }, Cmd.none, [] )

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

        IntelligenceMsg value ->
            ( { model | int = score value }, Cmd.none, [] )

        IntUp ->
            intUp model

        Locked value ->
            ( { model | locked = value }, Cmd.none, [] )

        Reroll ->
            reroll model

        StrDown ->
            strDown model

        StrengthMsg value ->
            ( { model | str = score value }, Cmd.none, [] )

        WisDown ->
            wisDown model

        WisdomMsg value ->
            ( { model | wis = score value }, Cmd.none, [] )

        WisUp ->
            wisUp model


subscriptions : Model -> Sub Msg
subscriptions model =
    dnd.subscriptions model.draggable


view : Model -> Html Msg
view model =
    let
        scoreRow :
            Stat
            -> String
            -> String
            -> (String -> Msg)
            -> Maybe Msg
            -> Maybe Msg
            -> Score
            -> List (Html Msg)
        scoreRow stat ability modifier inputMsg downMsg upMsg score =
            let
                typ =
                    if not model.locked then
                        "text"
                    else
                        "number"

                error =
                    if score.error then
                        [ ( "bg-danger text-white", True ) ]
                    else
                        []

                visible =
                    if model.locked then
                        input
                            [ classList <|
                                [ ( "form-control text-right w-100", True ) ]
                                    ++ error
                            , id ability
                            , Attributes.max "18"
                            , Attributes.min "1"
                            , onInput inputMsg
                            , style [ ( "min-width", "6rem" ) ]
                            , type_ typ
                            , value score.text
                            ]
                            []
                    else
                        dnd.droppable stat
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
                            ]
                            [ dnd.draggable ( stat, score.number )
                                []
                                [ h5
                                    [ style [ ( "width", "auto" ) ] ]
                                    [ text score.text ]
                                ]
                            ]
            in
                [ label
                    [ for ability
                    , style [ ( "grid-area", "auto / main-label / auto / score" ) ]
                    ]
                    [ text ability ]
                , div
                    [ style [ ( "grid-area", "auto / score / auto / down-arrow" ) ] ]
                    [ visible ]
                , div
                    [ style
                        [ ( "grid-area", "auto / down-arrow / auto / up-arrow" )
                        , ( "width", "40px" )
                        ]
                    ]
                    [ case downMsg of
                        Just msg ->
                            button
                                [ class "btn btn-outline-primary"
                                , hidden model.locked
                                , onClick msg
                                , type_ "button"
                                ]
                                [ i [ class "fas fa-arrow-down" ] []
                                ]

                        Nothing ->
                            div [ hidden model.locked ] []
                    ]
                , div
                    [ style
                        [ ( "grid-area", "auto / up-arrow / auto / mod-label" )
                        , ( "width", "40px" )
                        ]
                    ]
                    [ case upMsg of
                        Just msg ->
                            button
                                [ class "btn btn-outline-primary"
                                , hidden model.locked
                                , onClick msg
                                , type_ "button"
                                ]
                                [ i [ class "fas fa-arrow-up" ] []
                                ]

                        Nothing ->
                            div [ hidden model.locked ] []
                    ]
                , label
                    [ for modifier
                    , style [ ( "grid-area", "auto / mod-label / auto / mod" ) ]
                    ]
                    [ text modifier ]
                , div [ style [ ( "grid-area", "auto / mod / auto / end" ) ] ]
                    [ input
                        [ class "form-control text-right w-100"
                        , disabled True
                        , id modifier
                        , type_ "text"
                        , value score.mod
                        ]
                        []
                    ]
                ]
    in
        form [ class "border border-primary col-12 mt-1 p-2 rounded" ]
            [ h4 [ hidden model.locked ]
                [ text "Use the arrows or drag and drop to rearrange the scores." ]
            , div
                [ style
                    [ ( "align-items", "center" )
                    , ( "display", "grid" )
                    , ( "grid", "auto / [main-label] auto [score] 1fr [down-arrow] auto [up-arrow] auto [mod-label] auto [mod] 1fr [end]" )
                    , ( "grid-gap", "10px 15px" )
                    ]
                ]
              <|
                List.concat
                    [ scoreRow Strength "Strength" "STR" StrengthMsg (Just StrDown) Nothing model.str
                    , scoreRow Dexterity "Dexterity" "DEX" DexterityMsg (Just DexDown) (Just DexUp) model.dex
                    , scoreRow Constitution "Constitution" "CON" ConstitutionMsg (Just ConDown) (Just ConUp) model.con
                    , scoreRow Intelligence "Intelligence" "INT" IntelligenceMsg (Just IntDown) (Just IntUp) model.int
                    , scoreRow Wisdom "Wisdom" "WIS" WisdomMsg (Just WisDown) (Just WisUp) model.wis
                    , scoreRow Charisma "Charisma" "CHA" CharismaMsg Nothing (Just ChaUp) model.cha
                    ]
            , div [ class "form-group mt-2 row" ]
                [ div [ class "col-5" ]
                    [ div [ class "form-check" ]
                        [ input
                            [ attribute "data-toggle" "tooltip"
                            , attribute "data-placement" "bottom"
                            , class "form-check-input"
                            , id "locked"
                            , onCheck Locked
                            , title "Lock the scores so that they may not be exchanged or re-rolled. Individual scores may still be changed."
                            , type_ "checkbox"
                            ]
                            []
                        , label
                            [ attribute "data-toggle" "tooltip"
                            , attribute "data-placement" "bottom"
                            , class "form-check-label"
                            , for "locked"
                            , title "Lock the scores so that they may not be exchanged or re-rolled. Individual scores may still be changed."
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
            maybeScore model.cha.number

        cha =
            maybeScore model.wis.number
    in
        ( { model | wis = wis, cha = cha }
        , Cmd.none
        , [ Maybe.map CharismaUp cha.number, Maybe.map WisdomUp wis.number ]
            |> List.filterMap identity
        )


conDown : Model -> ( Model, Cmd Msg, List UpMsg )
conDown model =
    let
        con =
            maybeScore model.int.number
    in
        ( { model | con = con, int = maybeScore model.con.number }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.number ]
            |> List.filterMap identity
        )


conUp : Model -> ( Model, Cmd Msg, List UpMsg )
conUp model =
    let
        con =
            maybeScore model.dex.number
    in
        ( { model | con = con, dex = maybeScore model.con.number }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.number ]
            |> List.filterMap identity
        )


decoder : Seed -> Decoder Model
decoder seed =
    Pipeline.decode Model
        |> required "cha" decoderScore
        |> required "con" decoderScore
        |> required "dex" decoderScore
        |> hardcoded dnd.model
        |> required "int" decoderScore
        |> required "locked" bool
        |> hardcoded seed
        |> required "str" decoderScore
        |> required "wis" decoderScore


decoderScore : Decoder Score
decoderScore =
    Pipeline.decode Score
        |> required "error" bool
        |> required "mod" string
        |> optional "number" (maybe int) Nothing
        |> required "text" string


dexDown : Model -> ( Model, Cmd Msg, List UpMsg )
dexDown model =
    let
        con =
            maybeScore model.dex.number
    in
        ( { model | con = con, dex = maybeScore model.con.number }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.number ]
            |> List.filterMap identity
        )


dexUp : Model -> ( Model, Cmd Msg, List UpMsg )
dexUp model =
    ( { model
        | dex = maybeScore model.str.number
        , str = maybeScore model.dex.number
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
                        , Maybe.map ConstitutionUp value.number :: upMsgs
                        )

                    Intelligence ->
                        ( { model | int = value }, upMsgs )

                    Wisdom ->
                        ( { model | wis = value }
                        , Maybe.map WisdomUp value.number :: upMsgs
                        )

                    Charisma ->
                        ( { model | cha = value }
                        , Maybe.map CharismaUp value.number :: upMsgs
                        )

            ( model_, upMsgs ) =
                transfer to src ( model, [] )
                    |> transfer (Tuple.first from) dst
        in
            ( model_, Cmd.none, upMsgs |> List.filterMap identity )


encode : Model -> Value
encode model =
    Encode.object
        [ ( "cha", encodeScore model.cha )
        , ( "con", encodeScore model.con )
        , ( "dex", encodeScore model.dex )
        , ( "int", encodeScore model.int )
        , ( "locked", Encode.bool model.locked )
        , ( "str", encodeScore model.str )
        , ( "wis", encodeScore model.wis )
        ]


encodeScore : Score -> Value
encodeScore score =
    Encode.object
        [ ( "error", Encode.bool score.error )
        , ( "mod", Encode.string score.mod )
        , ( "number", EE.maybe Encode.int score.number )
        , ( "text", Encode.string score.text )
        ]


intDown : Model -> ( Model, Cmd Msg, List UpMsg )
intDown model =
    let
        wis =
            maybeScore model.int.number
    in
        ( { model | int = maybeScore model.wis.number, wis = wis }
        , Cmd.none
        , [ Maybe.map WisdomUp wis.number ]
            |> List.filterMap identity
        )


intUp : Model -> ( Model, Cmd Msg, List UpMsg )
intUp model =
    let
        con =
            maybeScore model.int.number
    in
        ( { model | con = con, int = maybeScore model.con.number }
        , Cmd.none
        , [ Maybe.map ConstitutionUp con.number ]
            |> List.filterMap identity
        )


maybeScore : Maybe Int -> Score
maybeScore value =
    { error = False
    , mod = modifiers value
    , number = value
    , text = Maybe.map toString value |> Maybe.withDefault ""
    }


score : String -> Score
score value =
    case String.toInt value of
        Ok n ->
            { error = False
            , mod = modifiers <| Just n
            , number = Just n
            , text = value
            }

        Err _ ->
            { error = True
            , mod = ""
            , number = Nothing
            , text = value
            }


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
            | cha = maybeScore <| Just rolls.cha
            , con = maybeScore <| Just rolls.con
            , dex = maybeScore <| Just rolls.dex
            , int = maybeScore <| Just rolls.int
            , seed = seed
            , str = maybeScore <| Just rolls.str
            , wis = maybeScore <| Just rolls.wis
          }
        , Cmd.none
        , [ CharismaUp rolls.cha, ConstitutionUp rolls.con, WisdomUp rolls.wis ]
        )


rollScores : Seed -> ( Rolls, Seed )
rollScores seed =
    let
        gen =
            R.list 4 (R.int 1 6)
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


strDown : Model -> ( Model, Cmd Msg, List UpMsg )
strDown model =
    ( { model
        | dex = maybeScore model.str.number
        , str = maybeScore model.dex.number
      }
    , Cmd.none
    , []
    )


wisDown : Model -> ( Model, Cmd Msg, List UpMsg )
wisDown model =
    let
        wis =
            maybeScore model.cha.number

        cha =
            maybeScore model.wis.number
    in
        ( { model | wis = wis, cha = cha }
        , Cmd.none
        , [ Maybe.map CharismaUp cha.number, Maybe.map WisdomUp wis.number ]
            |> List.filterMap identity
        )


wisUp : Model -> ( Model, Cmd Msg, List UpMsg )
wisUp model =
    let
        wis =
            maybeScore model.int.number
    in
        ( { model | int = maybeScore model.wis.number, wis = wis }
        , Cmd.none
        , [ Maybe.map WisdomUp wis.number ]
            |> List.filterMap identity
        )
