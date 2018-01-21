module Demographics exposing (Model, Msg(..), initialModel, update, view)

import Html
    exposing
        ( Html
        , button
        , div
        , form
        , h5
        , i
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
        , id
        , style
        , tabindex
        , title
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Ports
import Random.Pcg as R exposing (Seed, int, list, step)

type alias Model =
    { name : String
    , race : String
    , seed : Seed
    , selected : String
    }


type Msg
    = Add
    | Delete
    | Name String
    | Race String
    | Selected String
    | Yes


initialModel : Seed -> Model
initialModel seed =
    { name = ""
    , race = ""
    , seed = seed
    , selected = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            model ! []

        Delete ->
            model ! [ Ports.showDialog "#confirm-delete" ]

        Name value ->
            { model | name = value } ! []

        Race value ->
            { model | race = value } ! []

        Selected value ->
            model ! []

        Yes ->
            model ! []


view : Model -> Html Msg
view model =
    form
        [ class "border border-primary col-12 p-2 rounded"
        , style
            [ ( "align-items", "center" )
            , ( "display", "grid" )
            , ( "grid", "auto / auto 1fr auto" )
            , ( "grid-gap", "10px 15px" )
            ]
        ]
        [ label
            [ class "col-form-label"
            , for "select-character"
            , style [ ( "grid-area", "auto / 1 / auto / 2" ) ]
            ]
            [ text "Character" ]
        , div [ style [ ( "grid-area", "auto / 2 / auto / 3" ) ] ]
            [ select
                [ class "custom-select form-control"
                , id "select-character"
                , onInput Selected
                ]
                [ option [] [ text "Select a character" ]
                , option [ value "foo" ] [ text "Foo" ]
                , option [ value "bar" ] [ text "Bar" ]
                ]
            ]
        , div [ style [ ( "grid-area", "auto / 3 / auto / 4" ) ] ]
            [ button
                [ attribute "data-toggle" "tooltip"
                , attribute "data-placement" "bottom"
                , class "btn btn-primary btn-sm rounded-circle"
                , onClick Add
                , title "Add a new character."
                , type_ "button"
                ]
                [ i [ class "fas fa-plus" ] []
                ]
            ]
        , label
            [ class "col-form-label"
            , for "character-name"
            , style [ ( "grid-area", "auto / 1 / auto / 2" ) ]
            ]
            [ text "Name" ]
        , div [ style [ ( "grid-area", "auto / 2 / auto / 3" ) ] ]
            [ input
                [ class "w-100"
                , id "character-name"
                , onInput Name
                , type_ "text"
                , value model.name
                ]
                []
            ]
        , div [ style [ ( "grid-area", "auto / 3 / auto / 4" ) ] ]
            [ button
                [ attribute "data-toggle" "tooltip"
                , attribute "data-placement" "bottom"
                , class "btn btn-danger btn-sm rounded-circle"
                , onClick Delete
                , title "Delete this character. This cannot be undone."
                , type_ "button"
                ]
                [ i [ class "fas fa-trash" ] []
                ]
            ]
        , div
            [ attribute "aria-hidden" "true"
            , attribute "aria-labelledby" "confirmDeleteLabel"
            , attribute "role" "dialog"
            , class "modal fade"
            , id "confirm-delete"
            , tabindex -1
            ]
            [ div
                [ attribute "role" "document"
                , class "modal-dialog modal-dialog-centered"
                ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-header" ]
                        [ h5 [ class "modal-title" ] [ text "Delete this character?" ] ]
                    , div [ class "modal-body" ]
                        [ p [] [ text "This cannot be un-done!" ]
                        , p [] [ text "Hell will freeze over (not likely with global warming) and the universe will collapse back to the Big Bang singularity before you can recover this character, so think before answering..." ]
                        ]
                    , div [ class "modal-footer" ]
                        [ button
                            [ attribute "data-dismiss" "modal"
                            , class "btn btn-danger"
                            , onClick Yes
                            , type_ "button"
                            ]
                            [ text "Yes" ]
                        , button
                            [ attribute "data-dismiss" "modal"
                            , class "btn btn-primary"
                            , type_ "button"
                            ]
                            [ text "No" ]
                        ]
                    ]
                ]
            ]
        , label
            [ class "col-form-label"
            , for "character-race"
            , style [ ( "grid-area", "auto / 1 / auto / 2" ) ]
            ]
            [ text "Race" ]
        , div [ style [ ( "grid-area", "auto / 2 / auto / 3" ) ] ]
            [ input
                [ class "w-100"
                , id "character-race"
                , onInput Race
                , type_ "text"
                , value model.race
                ]
                []
            ]
        ]
