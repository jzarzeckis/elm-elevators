module Spawner exposing (step, seed)

import Time exposing (Time, second)
import Random exposing (Generator, bool, int, float, map4, andThen)
import DataTypes exposing (..)


seed : Random.Seed
seed =
    Random.initialSeed 0


minWait : Time
minWait =
    3 * second


maxWait : Time
maxWait =
    6 * second


nextPers : Time -> Int -> FloorNumber -> FloorNumber -> Bool -> Time -> NextPersonAdded
nextPers now floorCount floor target isMale delay =
    NextPersonAdded
        floor
        ((floor + target) % floorCount)
        (if isMale then
            Male
         else
            Female
        )
        ((now + delay) |> round |> toFloat)


nextPersonGenerator : Time -> Int -> Generator NextPersonAdded
nextPersonGenerator time floorCount =
    map4
        (nextPers time floorCount)
        (int 0 (floorCount - 1))
        (int 1 (floorCount - 1))
        bool
        (float minWait maxWait)


step : Time -> Int -> Random.Seed -> ( NextPersonAdded, Random.Seed )
step time floorCount seed =
    Random.step (nextPersonGenerator time floorCount) seed
