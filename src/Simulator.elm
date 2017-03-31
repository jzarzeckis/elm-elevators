module Simulator exposing (advance)

import DataTypes exposing (..)
import Time exposing (Time)
import Spawner
import People exposing (placeSpawnedPerson)


type NextEvent
    = ElevatorAdvance Elevator
    | SpawnerAdvance NextPersonAdded
    | EndOfUniverse


type alias Future =
    List NextEvent


getEta : NextEvent -> Time
getEta evt =
    case evt of
        ElevatorAdvance evt ->
            case evt.state of
                DoorsClosing t ->
                    t

                PeopleEntering t ->
                    t

                DoorsOpening t ->
                    t

                Moving _ t _ ->
                    t

                _ ->
                    1 / 0

        SpawnerAdvance evt ->
            evt.eta

        EndOfUniverse ->
            1 / 0


nearestEvent : Future -> NextEvent
nearestEvent =
    List.foldl
        (\e min ->
            if getEta e < getEta min then
                e
            else
                min
        )
        EndOfUniverse


futureEvents : Model -> Future
futureEvents model =
    (SpawnerAdvance model.nextComer)
        :: (model.elevators
                |> List.map ElevatorAdvance
           )


applyChange : Model -> NextEvent -> Model
applyChange model event =
    case event of
        EndOfUniverse ->
            Debug.crash "You've reached the end of universe"

        SpawnerAdvance nextPers ->
            let
                ( nextSpawn, seed ) =
                    Spawner.step nextPers.eta (List.length model.floors) model.randomSeed
            in
                case placeSpawnedPerson model.people nextPers model.time of
                    Ok person ->
                        { model
                            | time = nextPers.eta
                            , nextComer = nextSpawn
                            , randomSeed = seed
                            , people = person :: model.people
                        }

                    Err floor ->
                        { model
                            | phase = GameOver floor
                        }

        ElevatorAdvance el ->
            Debug.crash "Elevator advance hasn't been implemented"


advance : Model -> Time -> Model
advance model time =
    let
        e =
            model |> futureEvents |> nearestEvent

        eTime =
            getEta e

        now =
            time + model.time

        delta =
            now - eTime
    in
        if now < eTime then
            { model | time = now }
        else
            advance (applyChange model e) (delta)
