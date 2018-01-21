module Types exposing (Flags, Model, Msg(..), init)

import Alignment
import Bonds
import Demographics
import Equipment
import Health
import Random.Pcg as R exposing (independentSeed, initialSeed, step)
import Scores


type alias Flags =
    Int


type alias Model =
    { alignment : Alignment.Model
    , bonds : Bonds.Model
    , demographics : Demographics.Model
    , equipment : Equipment.Model
    , health : Health.Model
    , scores : Scores.Model
    }


type Msg
    = AlignmentMsg Alignment.Msg
    | BondsMsg Bonds.Msg
    | DemographicsMsg Demographics.Msg
    | EquipmentMsg Equipment.Msg
    | HealthMsg Health.Msg
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

        ( scores, rolls ) =
            Scores.initialModel scoresSeed
    in
        { alignment = Alignment.initialModel
        , bonds = Bonds.initialModel
        , demographics = Demographics.initialModel
        , equipment = Equipment.initialModel rolls.cha rolls.wis
        , health = Health.initialModel healthSeed rolls.con
        , scores = scores
        }
