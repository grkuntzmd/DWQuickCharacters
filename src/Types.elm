module Types exposing (Flags, Model, Msg(..), init)

import Health
import Random.Pcg as R exposing (independentSeed, initialSeed, step)
import Scores


type alias Flags =
    Int


type alias Model =
    { health : Health.Model
    , scores : Scores.Model
    }


type Msg
    = HealthMsg Health.Msg
    | ScoresMsg Scores.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags, Cmd.none )


initialModel : Flags -> Model
initialModel randomSeed =
    let
        seed =
            initialSeed randomSeed

        ( ( healthSeed, scoresSeed ), _ ) =
            step (R.map (,) independentSeed |> R.andMap independentSeed) seed
    in
        { health = Health.initialModel healthSeed
        , scores = Scores.initialModel scoresSeed
        }
