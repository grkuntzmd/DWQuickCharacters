module Main exposing (main)

import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h4
        , i
        , input
        , label
        , option
        , select
        , text
        )
import Html.Attributes exposing (class, for, id, type_)
import Html.Attributes.Aria exposing (ariaHidden)
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
            [ div [ class "col-lg-4 col-xl-2" ]
                [ demographics model ]
            , Html.map ScoresMsg <| Scores.view model.scores
            , h4 [ class "col-auto" ]
                [ text "Health" ]
            ]
        , div [ class "row" ]
            [ h4 [ class "col-auto" ]
                [ text "Equipment and Alignment" ]
            ]
        ]


demographics : Model -> Html Msg
demographics model =
    form []
        [ div [ class "form-group row" ]
            [ label
                [ class "col-form-label col-lg-4 col-xl-3"
                , for "select-character"
                ]
                [ text "Character" ]
            , div [ class "col-lg-6 col-xl-7" ]
                [ select
                    [ class "custom-select form-control"
                    , id "select-character"
                    ]
                    [ option [] [ text "Select a character" ] ]
                ]
            , div [ class "col-1" ]
                [ button [ class "btn btn-primary btn-sm rounded-circle" ]
                    [ i
                        [ ariaHidden True
                        , class "fa fa-plus"
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "form-group row" ]
            [ label
                [ class "col-form-label col-lg-4 col-xl-3"
                , for "character-name"
                ]
                [ text "Name" ]
            , div [ class "col-lg-6 col-xl-7" ]
                [ input
                    [ class "w-100"
                    , id "character-name"
                    , type_ "text"
                    ]
                    []
                ]
            , div [ class "col-1" ]
                [ button [ class "btn btn-danger btn-sm rounded-circle" ]
                    [ i
                        [ ariaHidden True
                        , class "fa fa-trash"
                        ]
                        []
                    ]
                ]
            ]
        , div [ class "form-group row" ]
            [ label
                [ class "col-form-label col-lg-4 col-xl-3"
                , for "character-race"
                ]
                [ text "Race" ]
            , div [ class "col-lg-6 col-xl-7" ]
                [ input
                    [ class "w-100"
                    , id "character-race"
                    , type_ "text"
                    ]
                    []
                ]
            ]
        ]
