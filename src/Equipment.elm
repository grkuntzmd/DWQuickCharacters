module Equipment
    exposing
        ( Model
        , Msg(..)
        , decoder
        , encode
        , initialModel
        , update
        , view
        )

import Dom
import Html exposing (Html, div, form, h3, input, label, text, textarea)
import Html.Attributes as Attributes
    exposing
        ( checked
        , class
        , for
        , id
        , name
        , placeholder
        , style
        , type_
        , value
        )
import Html.Events exposing (onBlur, onClick, onInput)
import Json.Decode exposing (Decoder, andThen, fail, int, maybe, string, succeed)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as EE
import Markdown
import Result exposing (Result(..))
import Task


type Class
    = Cleric
    | Fighter
    | Ranger
    | Thief
    | Wizard


type alias Model =
    { adventuringGear : Maybe Int
    , adventuringGearText : String
    , class : Class
    , coins : Maybe Int
    , coinsText : String
    , editing : Bool
    , otherItems : String
    , rations : Maybe Int
    , rationsText : String
    }


type Msg
    = AdventuringGear String
    | Charisma Int
    | ClassMsg Class
    | Coins String
    | Editing Bool
    | FocusOtherItems (Result Dom.Error ())
    | OtherItems String
    | Rations String
    | Wisdom Int


initialModel : Int -> Int -> Model
initialModel charisma wisdom =
    let
        adventuringGear =
            wisdom // 2

        coins =
            charisma

        rations =
            wisdom // 2
    in
        { adventuringGear = Just adventuringGear
        , adventuringGearText = toString adventuringGear
        , class = Fighter
        , coins = Just coins
        , coinsText = toString coins
        , editing = False
        , otherItems = ""
        , rations = Just rations
        , rationsText = toString rations
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdventuringGear value ->
            case String.toInt value |> Result.toMaybe of
                (Just _) as n ->
                    { model
                        | adventuringGear = n
                        , adventuringGearText = value
                    }
                        ! []

                Nothing ->
                    { model
                        | adventuringGear = Nothing
                        , adventuringGearText = model.adventuringGearText
                    }
                        ! []

        Charisma value ->
            { model
                | coins = Just value
                , coinsText = toString value
            }
                ! []

        ClassMsg value ->
            { model | class = value } ! []

        Coins value ->
            case String.toInt value |> Result.toMaybe of
                (Just _) as n ->
                    { model
                        | coins = n
                        , coinsText = value
                    }
                        ! []

                Nothing ->
                    { model
                        | coins = Nothing
                        , coinsText = model.coinsText
                    }
                        ! []

        Editing value ->
            let
                cmds =
                    if value then
                        [ Dom.focus "other-items"
                            |> Task.attempt FocusOtherItems
                        ]
                    else
                        []
            in
                { model | editing = value } ! cmds

        FocusOtherItems _ ->
            model ! []

        OtherItems value ->
            { model | otherItems = value } ! []

        Rations value ->
            case String.toInt value |> Result.toMaybe of
                (Just _) as n ->
                    { model
                        | rations = n
                        , rationsText = value
                    }
                        ! []

                Nothing ->
                    { model
                        | rations = Nothing
                        , rationsText = model.rationsText
                    }
                        ! []

        Wisdom value ->
            let
                adventuringGear =
                    value // 2

                rations =
                    value // 2
            in
                { model
                    | adventuringGear = Just adventuringGear
                    , adventuringGearText = toString adventuringGear
                    , rations = Just rations
                    , rationsText = toString rations
                }
                    ! []


view : Model -> Html Msg
view model =
    div [ class "border border-primary p-2 rounded" ]
        [ h3 [] [ text "Equipment" ]
        , form []
            [ div [ class "form-check" ]
                [ input
                    [ checked (model.class == Fighter)
                    , class "form-check-input"
                    , id "fighter"
                    , name "equipment"
                    , onClick (ClassMsg Fighter)
                    , type_ "radio"
                    , value "fighter"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "fighter"
                    ]
                    [ text "Sword (1d10 damage close) and chainmail (1 armor)" ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ checked (model.class == Ranger)
                    , class "form-check-input"
                    , id "ranger"
                    , name "equipment"
                    , onClick (ClassMsg Ranger)
                    , type_ "radio"
                    , value "ranger"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "ranger"
                    ]
                    [ text "Pistol, Bow or Throwing Knives (1d8 damage near) and 3 ammo" ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ checked (model.class == Wizard)
                    , class "form-check-input"
                    , id "wizard"
                    , name "equipment"
                    , onClick (ClassMsg Wizard)
                    , type_ "radio"
                    , value "wizard"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "wizard"
                    ]
                    [ text """Magic Missile spell (2d4 damage far, must defy
                        danger with INT to use) and dagger (1d4 damage close)""" ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ checked (model.class == Cleric)
                    , class "form-check-input"
                    , id "cleric"
                    , name "equipment"
                    , onClick (ClassMsg Cleric)
                    , type_ "radio"
                    , value "cleric"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "cleric"
                    ]
                    [ text """Cure Light Wounds spell (heal 1d8 damage, must
                        defy danger with WIS to use) and hammer (1d6 damage close)""" ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ checked (model.class == Thief)
                    , class "form-check-input"
                    , id "thief"
                    , name "equipment"
                    , onClick (ClassMsg Thief)
                    , type_ "radio"
                    , value "thief"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "thief"
                    ]
                    [ text """Stiletto (1d8 damage close), detect traps, pick locks
                        or pockets, or disable traps (must defy danger with DEX
                        to use)""" ]
                ]
            , div [ class "row mt-1" ]
                [ div
                    [ class "align-items-centers col-md-12 col-xl-5 d-grid"
                    , style
                        [ ( "grid", "auto / auto 1fr" )
                        , ( "grid-gap", "10px 10px" )
                        ]
                    ]
                    [ label [ class "grid-area-auto", for "adventuring-gear" ]
                        [ text "Adventuring Gear" ]
                    , div
                        [ class "grid-area-auto" ]
                        [ input
                            [ class "form-control text-right w-100"
                            , id "adventuring-gear"
                            , Attributes.min "0"
                            , onInput AdventuringGear
                            , type_ "number"
                            , value model.adventuringGearText
                            ]
                            []
                        ]
                    , label [ class "grid-area-auto", for "rations" ] [ text "Rations" ]
                    , div
                        [ class "grid-area-auto" ]
                        [ input
                            [ class "form-control text-right w-100"
                            , id "rations"
                            , Attributes.min "0"
                            , onInput Rations
                            , type_ "number"
                            , value model.rationsText
                            ]
                            []
                        ]
                    , label [ class "grid-area-auto", for "coins" ] [ text "Coins" ]
                    , div
                        [ class "grid-area-auto" ]
                        [ input
                            [ class "form-control text-right w-100"
                            , id "coins"
                            , Attributes.min "0"
                            , onInput Coins
                            , type_ "number"
                            , value model.coinsText
                            ]
                            []
                        ]
                    ]
                , div
                    [ class "col-md-12 col-xl-7 mt-md-1 mt-xl-0"
                    , onClick (Editing True)
                    ]
                    [ div [ class "align-items-stretch border border-primary d-flex flex-column h-100 justify-content-between p-1 rounded w-100" ]
                        [ let
                            visible =
                                if model.editing then
                                    textarea
                                        [ class "flex-1 form-control"
                                        , id "other-items"
                                        , onBlur (Editing False)
                                        , onInput OtherItems
                                        , placeholder "Other items..."
                                        , value model.otherItems
                                        ]
                                        []
                                else
                                    Markdown.toHtml
                                        [ class "flex-1" ]
                                        (if String.isEmpty model.otherItems then
                                            "Other Items... ([Markdown](https://daringfireball.net/projects/markdown/syntax) enabled)"
                                         else
                                            model.otherItems
                                        )
                          in
                            visible
                        ]
                    ]
                ]
            ]
        ]


decoder : Decoder Model
decoder =
    Pipeline.decode Model
        |> optional "adventuringGear" (maybe int) Nothing
        |> required "adventuringGearText" string
        |> required "class"
            (string
                |> andThen
                    (\str ->
                        case str of
                            "cleric" ->
                                succeed Cleric

                            "fighter" ->
                                succeed Fighter

                            "ranger" ->
                                succeed Ranger

                            "thief" ->
                                succeed Thief

                            "wizard" ->
                                succeed Wizard

                            _ ->
                                fail <| "unknown class"
                    )
            )
        |> optional "coins" (maybe int) Nothing
        |> required "coinsText" string
        |> hardcoded False
        |> required "otherItems" string
        |> optional "rations" (maybe int) Nothing
        |> required "rationsText" string


encode : Model -> Value
encode model =
    Encode.object
        [ ( "adventuringGear", EE.maybe Encode.int model.adventuringGear )
        , ( "adventuringGearText", Encode.string model.adventuringGearText )
        , ( "class"
          , case model.class of
                Cleric ->
                    Encode.string "cleric"

                Fighter ->
                    Encode.string "fighter"

                Ranger ->
                    Encode.string "ranger"

                Thief ->
                    Encode.string "thief"

                Wizard ->
                    Encode.string "wizard"
          )
        , ( "coins", EE.maybe Encode.int model.coins )
        , ( "coinsText", Encode.string model.coinsText )
        , ( "otherItems", Encode.string model.otherItems )
        , ( "rations", EE.maybe Encode.int model.rations )
        , ( "rationsText", Encode.string model.rationsText )
        ]
