module Scores exposing (Model, Msg(..), initialModel, update, view)

import Html exposing (Html, button, div, form, input, label, p, text)
import Html.Attributes exposing (class, for, id, readonly, type_, value)
import Html.Events exposing (onClick)
import Input.Number as IN
import List.Extra as LE
import Markdown
import Random.Pcg exposing (Seed, int, list, step)
import Result exposing (Result(..))


type Errors
    = ChaError
    | ConError
    | DexError
    | IntError
    | StrError
    | WisError


type alias Model =
    { cha : String
    , charisma : Int
    , con : String
    , constitution : Int
    , dex : String
    , dexterity : Int
    , int : String
    , intelligence : Int
    , scores : List Int
    , seed : Seed
    , str : String
    , strength : Int
    , usedScores : List Int
    , wis : String
    , wisdom : Int
    }


type Msg
    = Reroll
    | Strength String


type Score
    = ChaScore Int
    | ConScore Int
    | DexScore Int
    | IntScore Int
    | StrScore Int
    | WisScore Int


initialModel : Seed -> Model
initialModel seed =
    let
        ( scores, seed_ ) =
            rollScores seed
    in
        { cha = ""
        , charisma = 10
        , con = ""
        , constitution = 10
        , dex = ""
        , dexterity = 10
        , int = ""
        , intelligence = 10
        , scores = scores
        , seed = seed_
        , str = ""
        , strength = 10
        , usedScores = []
        , wis = ""
        , wisdom = 10
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reroll ->
            let
                ( scores, seed ) =
                    rollScores model.seed
            in
                { model | scores = scores, seed = seed } ! []

        Strength value ->
            let
                _ =
                    Debug.log "value" value
            in
                model ! []


view : Model -> Html Msg
view model =
    div [ class "col-lg-6 col-xl-6" ]
        [ div [ class "row" ]
            [ form [ class "col-lg-4 col-xl-2" ]
                [ div [ class "form-group" ]
                    [ label [ for "strength" ] [ text "Strength" ]
                    , IN.inputString
                        { hasFocus = Nothing
                        , maxLength = Nothing
                        , maxValue = Just 18
                        , minValue = Just 3
                        , onInput = Strength
                        }
                        [ class "w-100", id "strength", type_ "number" ]
                        (toString model.strength)
                    ]
                , div [ class "form-group" ]
                    [ label [ class "m-2 ", for "str" ] [ text "STR" ]
                    , input [ class "w-50", id "str", readonly True, type_ "text" ] []
                    ]
                ]
            , form [ class "col-lg-4 col-xl-2" ]
                [ div [ class "form-group" ]
                    [ label [ for "dexterity" ] [ text "Dexterity" ]
                    , input [ class "w-100", id "dexterity", type_ "number" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "m-2 ", for "dex" ] [ text "DEX" ]
                    , input [ class "w-50", id "dex", readonly True, type_ "text" ] []
                    ]
                ]
            , form [ class "col-lg-4 col-xl-2" ]
                [ div [ class "form-group" ]
                    [ label [ for "constitution" ] [ text "Constitution" ]
                    , input [ class "w-100", id "constitution", type_ "number" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "m-2", for "con" ] [ text "CON" ]
                    , input [ class "w-50", id "con", readonly True, type_ "text" ] []
                    ]
                ]
            , form [ class "col-lg-4 col-xl-2" ]
                [ div [ class "form-group" ]
                    [ label [ for "intelligence" ] [ text "Intelligence" ]
                    , input [ class "w-100", id "intelligence", type_ "number" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "m-2", for "int" ] [ text "INT" ]
                    , input [ class "w-50", id "int", readonly True, type_ "text" ] []
                    ]
                ]
            , form [ class "col-lg-4 col-xl-2" ]
                [ div [ class "form-group" ]
                    [ label [ for "wisdom" ] [ text "Wisdom" ]
                    , input [ class "w-100", id "wisdom", type_ "number" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "m-2", for "wis" ] [ text "WIS" ]
                    , input [ class "w-50", id "wis", readonly True, type_ "text" ] []
                    ]
                ]
            , form [ class "col-lg-4 col-xl-2" ]
                [ div [ class "form-group" ]
                    [ label [ for "charisma" ] [ text "Charisma" ]
                    , input [ class "w-100", id "charisma", type_ "number" ] []
                    ]
                , div [ class "form-group" ]
                    [ label [ class "m-2", for "cha" ] [ text "CHA" ]
                    , input [ class "w-50", id "cha", readonly True, type_ "text" ] []
                    ]
                ]
            ]
        , div [ class "align-items-center row" ]
            [ p [ class "col-lg-5 col-xl-8" ] [ text """
              Enter the following numbers distributed among the scores in any
              order, using each number only once.
            """ ]
            , input
                [ class "col-lg-4 col-xl-2"
                , readonly True
                , List.map toString model.scores
                    |> String.join ", "
                    |> value
                ]
                []
            , button
                [ class "btn btn-primary btn-sm col-lg-2 col-xl-1 ml-1"
                , onClick Reroll
                ]
                [ text "Re-roll" ]
            ]
        ]


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


usedScores : List Int -> List Score -> Result Errors (List Int)
usedScores scores used =
    let
        removeIf : Int -> List Int -> Errors -> Result Errors (List Int)
        removeIf v s msg =
            case LE.find ((==) v) s of
                Just _ ->
                    Ok <| negate v :: LE.remove v s

                Nothing ->
                    Err msg
    in
        List.foldl
            (\u r ->
                case ( u, r ) of
                    ( ChaScore v, Ok s ) ->
                        removeIf v s ChaError

                    ( ConScore v, Ok s ) ->
                        removeIf v s ConError

                    ( DexScore v, Ok s ) ->
                        removeIf v s DexError

                    ( IntScore v, Ok s ) ->
                        removeIf v s IntError

                    ( StrScore v, Ok s ) ->
                        removeIf v s StrError

                    ( WisScore v, Ok s ) ->
                        removeIf v s WisError

                    ( _, Err _ ) ->
                        r
            )
            (Ok scores)
            used
