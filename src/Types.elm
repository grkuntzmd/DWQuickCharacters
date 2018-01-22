module Types exposing (Flags, Model, Msg(..), init, initialModel)

import Alignment
import Bonds
import Demographics
import Equipment
import Health
import Ports exposing (loadNames)
import Random.Pcg as R exposing (Seed, independentSeed, initialSeed, step)
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
    , seed : Seed
    }


type Msg
    = AlignmentMsg Alignment.Msg
    | BondsMsg Bonds.Msg
    | DemographicsMsg Demographics.Msg
    | EquipmentMsg Equipment.Msg
    | GetCharacter String
    | GetNames (List ( String, String ))
    | HealthMsg Health.Msg
    | ScoresMsg Scores.Msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel (initialSeed flags), loadNames () )


initialModel : Seed -> Model
initialModel seed =
    let
        ( ( demographicsSeed, healthSeed, scoresSeed ), seed_ ) =
            step
                (R.map (,,) independentSeed
                    |> R.andMap independentSeed
                    |> R.andMap independentSeed
                )
                seed

        ( scores, rolls ) =
            Scores.initialModel scoresSeed
    in
        { alignment = Alignment.initialModel
        , bonds = Bonds.initialModel
        , demographics = Demographics.initialModel demographicsSeed
        , equipment = Equipment.initialModel rolls.cha rolls.wis
        , health = Health.initialModel healthSeed rolls.con
        , scores = scores
        , seed = seed_
        }
