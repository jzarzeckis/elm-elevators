module People exposing (..)

import DataTypes exposing (..)
import Time exposing (Time)


{- Checks floor for available spots and
   returns a new Person at the first available spot
-}


placeSpawnedPerson : People -> NextPersonAdded -> Time -> Result FloorNumber Person
placeSpawnedPerson people spawned born =
    let
        obstacles =
            takenPositions spawned.floor people
    in
        List.range 0 15
            |> List.foldl
                (\pos res ->
                    case res of
                        Ok _ ->
                            res

                        _ ->
                            if List.member pos obstacles then
                                res
                            else
                                Ok <|
                                    Person
                                        spawned.targetFloor
                                        (Waiting pos spawned.floor)
                                        spawned.gender
                                        (toString born)
                )
                (Err spawned.floor)


takenPositions : FloorNumber -> People -> List Int
takenPositions fl =
    List.filterMap
        (\p ->
            case p.state of
                Waiting pos nr ->
                    if nr == fl then
                        Just pos
                    else
                        Nothing

                _ ->
                    Nothing
        )


peopleOnFloor : FloorNumber -> People -> People
peopleOnFloor fl =
    List.filter
        (\p ->
            case p.state of
                Waiting _ nr ->
                    nr == fl

                _ ->
                    False
        )
