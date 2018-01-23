module Main exposing (main)

import Alignment
import Bonds
import Demographics
import Equipment
import Health
import Html exposing (Html, a, button, div, h5, img, p, text)
import Html.Attributes
    exposing
        ( attribute
        , class
        , href
        , id
        , src
        , style
        , tabindex
        , target
        , type_
        )
import Json.Decode exposing (decodeString)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, required)
import Json.Encode as Encode exposing (Value)
import Moves
import Ports
    exposing
        ( deleteItem
        , getItem
        , getNames
        , loadItem
        , loadNames
        , saveItem
        )
import Random.Pcg as R exposing (independentSeed, step)
import Scores
import Types exposing (Flags, Model, Msg(..), init, initialModel)


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
    let
        ( model_, cmd ) =
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

                DemographicsMsg msg_ ->
                    let
                        ( model_, cmd, upMsg ) =
                            Demographics.update msg_ model.demographics
                    in
                        demographicsUpMsg upMsg cmd { model | demographics = model_ }

                EquipmentMsg msg_ ->
                    let
                        ( model_, cmd ) =
                            Equipment.update msg_ model.equipment
                    in
                        { model | equipment = model_ } ! [ Cmd.map EquipmentMsg cmd ]

                GetCharacter value ->
                    decode value model ! []

                GetNames items ->
                    let
                        ( model_, cmd, upMsg ) =
                            Demographics.update
                                (Demographics.Names <|
                                    List.sortBy Tuple.second items
                                )
                                model.demographics
                    in
                        demographicsUpMsg upMsg cmd { model | demographics = model_ }

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

        -- _ =
        --     Debug.log "Main msg, update, cmd" ( msg, model_, cmd )
    in
        ( model_
        , Cmd.batch
            [ cmd
            , if String.isEmpty model_.demographics.name then
                Cmd.none
              else
                saveItem
                    ( model_.demographics.uuid
                    , Encode.encode 0 <| encode model_
                    )
            ]
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map ScoresMsg
            (Scores.subscriptions model.scores)
        , getItem GetCharacter
        , getNames GetNames
        ]


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ header
        , div [ class "align-items-stretch justify-content-between row" ]
            [ div [ class "col-md-6 col-xl-5" ]
                [ div [ class "container-fluid" ]
                    [ div [ class "row" ]
                        [ Html.map DemographicsMsg <|
                            Demographics.view model.demographics
                        ]
                    , div [ class "row" ]
                        [ Html.map ScoresMsg <| Scores.view model.scores ]
                    , div [ class "row" ]
                        [ Html.map HealthMsg <| Health.view model.health ]
                    ]
                ]
            , div [ class "align-items-stretch col-md-6 col-xl-7 d-flex flex-column justify-content-between" ]
                [ Html.map EquipmentMsg <| Equipment.view model.equipment
                , Html.map AlignmentMsg <| Alignment.view model.alignment
                , Html.map BondsMsg <| Bonds.view model.bonds
                ]
            ]
        , Html.map ScoresMsg (Scores.dragged model.scores)
        , Moves.view
        ]


decode : String -> Model -> Model
decode value model =
    let
        ( ( demographicsSeed, healthSeed, scoresSeed ), seed_ ) =
            step
                (R.map (,,) independentSeed
                    |> R.andMap independentSeed
                    |> R.andMap independentSeed
                )
                model.seed
    in
        case
            decodeString
                (Pipeline.decode Model
                    |> required "alignment" Alignment.decoder
                    |> required "bonds" Bonds.decoder
                    |> required "demographics"
                        (Demographics.decoder
                            demographicsSeed
                            model.demographics
                        )
                    |> required "equipment" Equipment.decoder
                    |> required "health" (Health.decoder healthSeed)
                    |> required "scores" (Scores.decoder scoresSeed)
                    |> hardcoded seed_
                )
                value
        of
            Ok model_ ->
                model_

            Err _ ->
                initialModel model.seed


demographicsUpMsg :
    Demographics.UpMsg
    -> Cmd Demographics.Msg
    -> Model
    -> ( Model, Cmd Msg )
demographicsUpMsg upMsg cmd model =
    case upMsg of
        Demographics.AddUp ->
            ( initialModel model.seed
            , Cmd.batch [ Cmd.map DemographicsMsg cmd, loadNames () ]
            )

        Demographics.DeleteUp id ->
            ( initialModel model.seed
            , Cmd.batch [ Cmd.map DemographicsMsg cmd, deleteItem id ]
            )

        Demographics.NoneUp ->
            ( model, Cmd.map DemographicsMsg cmd )

        Demographics.SelectUp id ->
            ( model, loadItem id )


encode : Model -> Value
encode model =
    Encode.object
        [ ( "alignment", Alignment.encode model.alignment )
        , ( "bonds", Bonds.encode model.bonds )
        , ( "demographics", Demographics.encode model.demographics )
        , ( "equipment", Equipment.encode model.equipment )
        , ( "health", Health.encode model.health )
        , ( "scores", Scores.encode model.scores )
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
                    , class "modal-dialog modal-dialog-centered modal-lg"
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
