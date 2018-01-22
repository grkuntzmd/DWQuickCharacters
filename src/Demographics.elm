module Demographics
    exposing
        ( Model
        , Msg(..)
        , UpMsg(..)
        , decoder
        , encode
        , initialModel
        , update
        , view
        )

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
        , classList
        , for
        , id
        , style
        , tabindex
        , title
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline as Pipeline exposing (hardcoded, required)
import Json.Encode as Encode exposing (Value)
import Ports exposing (showDialog)
import Random.Pcg exposing (Seed, step)
import Result exposing (Result(..))
import Uuid


type alias Model =
    { error : Bool
    , name : String
    , names : List ( String, String )
    , race : String
    , seed : Seed
    , selected : String
    , uuid : String
    }


type Msg
    = Add
    | Delete
    | Names (List ( String, String ))
    | Name String
    | Race String
    | Selected String
    | Yes


type UpMsg
    = AddUp
    | NoneUp
    | SelectUp String


initialModel : Seed -> Model
initialModel seed =
    let
        ( uuid, seed_ ) =
            step Uuid.uuidGenerator seed
                |> Tuple.mapFirst Uuid.toString
    in
        { error = True
        , name = ""
        , names = []
        , race = ""
        , seed = seed_
        , selected = ""
        , uuid = uuid
        }


update : Msg -> Model -> ( Model, Cmd Msg, UpMsg )
update msg model =
    case msg of
        Add ->
            ( model, Cmd.none, AddUp )

        Delete ->
            ( model, showDialog "#confirm-delete", NoneUp )

        Name value ->
            if String.isEmpty value then
                ( { model | error = True, name = value }, Cmd.none, NoneUp )
            else
                ( { model | error = False, name = value }, Cmd.none, NoneUp )

        Names items ->
            ( { model | names = items }, Cmd.none, NoneUp )

        Race value ->
            ( { model | race = value }, Cmd.none, NoneUp )

        Selected value ->
            ( model, Cmd.none, SelectUp value )

        Yes ->
            ( model, Cmd.none, NoneUp )


view : Model -> Html Msg
view model =
    let
        error =
            if model.error then
                [ ( "bg-danger text-white", True ) ]
            else
                []
    in
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
                  <|
                    option [] [ text "Select a character" ]
                        :: List.map
                            (\( id, name ) ->
                                option [ value id ] [ text name ]
                            )
                            model.names
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
                    [ classList <| [ ( "w-100", True ) ] ++ error
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


decoder : Seed -> Model -> Decoder Model
decoder seed model =
    Pipeline.decode Model
        |> required "error" bool
        |> required "name" string
        |> hardcoded model.names
        |> required "race" string
        |> hardcoded seed
        |> hardcoded ""
        |> required "uuid" string


encode : Model -> Value
encode model =
    Encode.object
        [ ( "error", Encode.bool model.error )
        , ( "name", Encode.string model.name )
        , ( "race", Encode.string model.race )
        , ( "uuid", Encode.string model.uuid )
        ]
