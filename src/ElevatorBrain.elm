module ElevatorBrain exposing (destinationQueue)

import DataTypes exposing (..)
import Dict


type alias FloorButtonsPushed =
    Dict FloorNumber ( Bool, Bool )


type alias Elevators =
    Dict ElevatorNumber ( Set FloorNumber, Float, ElevatorState )


type alias Destinations =
    Dict ElevatorNumber Floors


destinationQueue : FloorButtonsPushed -> PassengerIntentions -> Destinations -> Destinations
destinationQueue buttons intentions prev =
    prev
        |> Dict.map (always [ 0 ])
