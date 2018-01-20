module Alignment exposing (Model, Msg(..), initialModel, update, view)

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


type alias Model =
    {}


type Msg
    = None


initialModel : Model
initialModel =
    {}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            model ! []


view : Model -> Html Msg
view model =
    div [ class "border border-primary col-12 mt-1 p-2 rounded" ]
        [ h2 [] [ text "Alignment" ]
        ]
