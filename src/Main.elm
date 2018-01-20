module Main exposing (main)

import Alignment
import Bonds
import Equipment
import Health
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , form
        , h5
        , i
        , img
        , input
        , label
        , option
        , p
        , select
        , text
        )
import Html.Attributes
    exposing
        ( attribute
        , class
        , for
        , href
        , id
        , src
        , style
        , tabindex
        , target
        , title
        , type_
        )
import Moves
import Scores
import Types exposing (Flags, Model, Msg(..), init)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- Tuple.second <|
    --     Debug.log "Main msg, update"
    --         ( msg
    --         ,
    case msg of
        AlignmentMsg msg_ ->
            let
                ( model_, cmd ) =
                    Alignment.update msg_ model.alignment
            in
                { model | alignment = model_ } ! [ Cmd.map AlignmentMsg cmd ]

        BondsMsg msg_ ->
            let
                ( model_, cmd ) =
                    Bonds.update msg_ model.bonds
            in
                { model | bonds = model_ } ! [ Cmd.map BondsMsg cmd ]

        EquipmentMsg msg_ ->
            let
                ( model_, cmd ) =
                    Equipment.update msg_ model.equipment
            in
                { model | equipment = model_ } ! [ Cmd.map EquipmentMsg cmd ]

        HealthMsg msg_ ->
            let
                ( model_, cmd ) =
                    Health.update msg_ model.health
            in
                { model | health = model_ } ! [ Cmd.map HealthMsg cmd ]

        ScoresMsg msg_ ->
            let
                ( scoresModel, scoresCmd, upMsgs ) =
                    Scores.update msg_ model.scores

                ( model_, cmds ) =
                    List.foldl
                        (\upMsg ( model, cmds ) ->
                            case upMsg of
                                Scores.CharismaUp value ->
                                    let
                                        ( model_, cmd ) =
                                            Equipment.update
                                                (Equipment.Charisma value)
                                                model.equipment
                                    in
                                        ( { model | equipment = model_ }
                                        , Cmd.map EquipmentMsg cmd :: cmds
                                        )

                                Scores.ConstitutionUp value ->
                                    let
                                        ( model_, cmd ) =
                                            Health.update
                                                (Health.Constitution value)
                                                model.health
                                    in
                                        ( { model | health = model_ }
                                        , Cmd.map HealthMsg cmd :: cmds
                                        )

                                Scores.WisdomUp value ->
                                    let
                                        ( model_, cmd ) =
                                            Equipment.update
                                                (Equipment.Wisdom value)
                                                model.equipment
                                    in
                                        ( { model | equipment = model_ }
                                        , Cmd.map EquipmentMsg cmd :: cmds
                                        )
                        )
                        ( { model | scores = scoresModel }
                        , [ Cmd.map ScoresMsg scoresCmd ]
                        )
                        upMsgs
            in
                model_ ! cmds



-- )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ScoresMsg
        (Scores.subscriptions model.scores)


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ header
        , div [ class "row" ]
            [ div [ class "col-md-6 col-xl-5" ]
                [ div [ class "container-fluid" ]
                    [ div [ class "row" ]
                        [ div [ class "border border-primary col-12 mt-1 p-2 rounded" ]
                            [ demographics model ]
                        ]
                    , div [ class "row" ]
                        [ Html.map ScoresMsg <| Scores.view model.scores ]
                    , div [ class "row" ]
                        [ Html.map HealthMsg <| Health.view model.health ]
                    ]
                ]
            , div [ class "col-md-6 col-xl-7" ]
                [ div [ class "row" ]
                    [ div [ class "container-fluid" ]
                        [ Html.map EquipmentMsg <| Equipment.view model.equipment ]
                    ]
                , div [ class "row" ]
                    [ div [ class "container-fluid" ]
                        [ Html.map AlignmentMsg <| Alignment.view model.alignment ]
                    ]
                , div [ class "row" ]
                    [ div [ class "container-fluid" ]
                        [ Html.map BondsMsg <| Bonds.view model.bonds ]
                    ]
                ]
            ]
        , Html.map ScoresMsg (Scores.dragged model.scores)
        , Moves.view
        ]


demographics : Model -> Html Msg
demographics model =
    form [ class "w-100" ]
        [ div [ class "form-group row" ]
            [ label
                [ class "col-form-label col-3"
                , for "select-character"
                ]
                [ text "Character" ]
            , div [ class "col-7" ]
                [ select
                    [ class "custom-select form-control"
                    , id "select-character"
                    ]
                    [ option [] [ text "Select a character" ] ]
                ]
            , div [ class "col-1" ]
                [ button [ class "btn btn-primary btn-sm rounded-circle" ]
                    [ i [ class "fas fa-plus" ] []
                    ]
                ]
            ]
        , div [ class "form-group row" ]
            [ label
                [ class "col-form-label col-3"
                , for "character-name"
                ]
                [ text "Name" ]
            , div [ class "col-7" ]
                [ input
                    [ class "w-100"
                    , id "character-name"
                    , type_ "text"
                    ]
                    []
                ]
            , div [ class "col-1" ]
                [ button
                    [ attribute "data-toggle" "tooltip"
                    , attribute "data-placement" "bottom"
                    , class "btn btn-danger btn-sm rounded-circle"
                    , title "Delete this character. This cannot be undone."
                    ]
                    [ i [ class "fas fa-trash" ] []
                    ]
                ]
            ]
        , div [ class "form-group row" ]
            [ label
                [ class "col-form-label col-3"
                , for "character-race"
                ]
                [ text "Race" ]
            , div [ class "col-7" ]
                [ input
                    [ class "w-100"
                    , id "character-race"
                    , type_ "text"
                    ]
                    []
                ]
            ]
        ]


header : Html Msg
header =
    div [ class "container-fluid" ]
        [ div [ class "border border-primary py-1 rounded row" ]
            [ div
                [ class "col-md-5 col-xl-4"
                , style [ ( "display", "inline-block" ) ]
                ]
                [ img
                    [ src "logo.svg"
                    , style
                        [ ( "height", "30px" )
                        , ( "width", "auto" )
                        ]
                    ]
                    []
                , h5
                    [ class "mt-2"
                    , style [ ( "display", "inline-block" ) ]
                    ]
                    [ text "Quick Characters" ]
                ]
            , div [ class "col-md-5 col-xl-4 offset-md-2 offset-xl-4 text-right" ]
                [ button
                    [ attribute "data-target" "#license"
                    , attribute "data-toggle" "modal"
                    , class "btn btn-outline-primary"
                    , type_ "button"
                    ]
                    [ text "Copyright Ⓒ 2018, G. Ralph Kuntz, MD" ]
                ]
            , div
                [ attribute "aria-hidden" "true"
                , attribute "aria-labelledby" "licenseLabel"
                , attribute "role" "dialog"
                , class "modal fade"
                , id "license"
                , tabindex -1
                ]
                [ div
                    [ attribute "role" "document"
                    , class "modal-dialog"
                    , style
                        [ ( "min-width", "40rem" ) ]
                    ]
                    [ div [ class "modal-content" ]
                        [ div [ class "modal-header" ]
                            [ h5 [ class "modal-title" ] [ text "Apache 2.0 License" ] ]
                        , div [ class "modal-body" ]
                            [ p []
                                [ text "Copyright 2018, G. Ralph Kuntz, MD" ]
                            , p []
                                [ text """Licensed under the Apache License, Version 2.0
                                (the "License"); you may not use this file except
                                in compliance with the License. You may obtain a
                                copy of the License at""" ]
                            , p []
                                [ a
                                    [ href "http://www.apache.org/licenses/LICENSE-2.0"
                                    , target "_blank"
                                    ]
                                    [ text "http://www.apache.org/licenses/LICENSE-2.0" ]
                                ]
                            , p []
                                [ text """Unless required by applicable law or agreed
                                to in writing, software distributed under the
                                License is disheadertributed on an "AS IS" BASIS,
                                WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
                                either express or implied. See the License for
                                the specific language governing permissions and
                                limitations under the License.""" ]
                            , p []
                                [ text "Source code available at "
                                , a
                                    [ href "https://github.com/grkuntzmd/DWQuickCharacters"
                                    , target "_blank"
                                    ]
                                    [ text "https://github.com/grkuntzmd/DWQuickCharacters" ]
                                ]
                            ]
                        , div [ class "modal-footer" ]
                            [ button
                                [ attribute "data-dismiss" "modal"
                                , class "btn btn-primary"
                                , type_ "button"
                                ]
                                [ text "Close" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
