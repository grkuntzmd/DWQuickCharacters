module Main exposing (main)

import Health
import Html
    exposing
        ( Html
        , button
        , div
        , form
        , i
        , input
        , label
        , option
        , select
        , text
        )
import Html.Attributes exposing (class, for, id, type_)
import Moves
import Scores
import Types exposing (Flags, Model, Msg(..), init)


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    Tuple.second <|
        Debug.log "Main msg, update"
            ( msg
            , case msg of
                HealthMsg msg_ ->
                    let
                        ( model_, cmd ) =
                            Health.update msg_ model.health
                    in
                        { model | health = model_ } ! [ Cmd.map HealthMsg cmd ]

                ScoresMsg msg_ ->
                    let
                        ( model_, cmd ) =
                            Scores.update msg_ model.scores
                    in
                        { model | scores = model_ } ! [ Cmd.map ScoresMsg cmd ]
            )


view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ div [ class "row" ]
            [ div [ class "border border-primary col-md-6 col-xl-4 p-2 rounded" ]
                [ demographics model ]
            ]
        , div [ class "row" ]
            [ Html.map ScoresMsg <| Scores.view model.scores ]
        , div [ class "row" ]
            [ Html.map HealthMsg <| Health.view model.health ]
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
                [ button [ class "btn btn-danger btn-sm rounded-circle" ]
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
