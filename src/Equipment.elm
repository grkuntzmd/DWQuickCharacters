module Equipment exposing (Model, Msg(..), initialModel, update, view)

import Dom
import Html
    exposing
        ( Html
        , a
        , div
        , form
        , h2
        , i
        , input
        , label
        , text
        , textarea
        )
import Html.Attributes as Attributes
    exposing
        ( class
        , for
        , hidden
        , href
        , id
        , name
        , placeholder
        , type_
        , value
        )
import Html.Events exposing (onBlur, onClick, onInput)
import Markdown
import Result exposing (Result(..))
import Task


type alias Model =
    { adventuringGear : Maybe Int
    , adventuringGearText : String
    , coins : Maybe Int
    , coinsText : String
    , editing : Bool
    , otherItems : String
    , rations : Maybe Int
    , rationsText : String
    }


type Msg
    = Charisma Int
    | Editing Bool
    | FocusOtherItems (Result Dom.Error ())
    | OtherItems String
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
        Charisma value ->
            { model
                | coins = Just value
                , coinsText = toString value
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
    div [ class "border border-primary col-12 mt-1 p-2 rounded" ]
        [ h2 [] [ text "Equipment" ]
        , form []
            [ div [ class "form-check" ]
                [ input
                    [ class "form-check-input"
                    , id "sword"
                    , name "equipment"
                    , type_ "radio"
                    , value "sword"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "sword"
                    ]
                    [ text "Sword (1d8 damage close) and chainmail (1 armor)" ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ class "form-check-input"
                    , id "pistol"
                    , name "equipment"
                    , type_ "radio"
                    , value "pistol"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "pistol"
                    ]
                    [ text "Pistol, Bow or Throwing Knives (1d8 damage near) and 3 ammo" ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ class "form-check-input"
                    , id "magic-missile"
                    , name "equipment"
                    , type_ "radio"
                    , value "magic-missile"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "magic-missile"
                    ]
                    [ text """Magic Missile spell (2d4 damage far, must defy
                        danger with INT to use) and dagger (1d4 damage close)""" ]
                ]
            , div [ class "form-check" ]
                [ input
                    [ class "form-check-input"
                    , id "hammer"
                    , name "equipment"
                    , type_ "radio"
                    , value "hammer"
                    ]
                    []
                , label
                    [ class "form-check-label"
                    , for "hammer"
                    ]
                    [ text """Cure Light Wounds spell (heal 1d8 damage, must
                        defy danger with WIS to use) and hammer (1d6 damage close)""" ]
                ]
            , div [ class "row mt-1" ]
                [ div [ class "col-md-12 col-xl-5 container-fluid" ]
                    [ div [ class "row" ]
                        [ label [ class "col-7", for "adventuring-gear" ] [ text "Adventuring Gear" ]
                        , div [ class "col-5" ]
                            [ input
                                [ class "form-control text-right w-100"
                                , id "adventuring-gear"
                                , Attributes.min "0"
                                , type_ "number"
                                , value model.adventuringGearText
                                ]
                                []
                            ]
                        ]
                    , div [ class "mt-1 row" ]
                        [ label [ class "col-7", for "rations" ] [ text "Rations" ]
                        , div [ class "col-5" ]
                            [ input
                                [ class "form-control text-right w-100"
                                , id "rations"
                                , Attributes.min "0"
                                , type_ "number"
                                , value model.rationsText
                                ]
                                []
                            ]
                        ]
                    , div [ class "mt-1 row" ]
                        [ label [ class "col-7", for "coins" ] [ text "Coins" ]
                        , div [ class "col-5" ]
                            [ input
                                [ class "form-control text-right w-100"
                                , id "coins"
                                , Attributes.min "0"
                                , type_ "number"
                                , value model.coinsText
                                ]
                                []
                            ]
                        ]
                    ]
                , div
                    [ class "col-md-12 col-xl-7 mt-md-1 mt-xl-0"
                    , onClick (Editing True)
                    ]
                    [ div [ class "border border-primary h-100 p-1 rounded w-100" ]
                        [ Markdown.toHtml
                            [ hidden model.editing ]
                            (if String.isEmpty model.otherItems then
                                """Other Items... ([Markdown](https://daringfireball.net/projects/markdown/syntax)
                                  allowed)"""
                             else
                                model.otherItems
                            )
                        , textarea
                            [ class "form-control h-100 w-100"
                            , hidden <| not model.editing
                            , id "other-items"
                            , onBlur (Editing False)
                            , onInput OtherItems
                            , placeholder "Other items..."
                            , value model.otherItems
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]