module Types exposing (Flags, Model, Msg(..), init)

import Random.Pcg exposing (initialSeed)
import Scores


type alias Flags =
    Int


type alias Model =
    { scores : Scores.Model }


type Msg
    = ScoresMsg Scores.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags, Cmd.none )


initialModel : Flags -> Model
initialModel randomSeed =
    { scores = Scores.initialModel <| initialSeed randomSeed }
