module DataTypes exposing (..)

import Set exposing (Set)
import Time exposing (Time, millisecond, second)
import Random


floorHeight : Float
floorHeight =
    50


elevatorWidth : Float
elevatorWidth =
    40


peopleSpacing : Float
peopleSpacing =
    13


acceleration : Float
acceleration =
    -- px/ms²
    0.001


maxSpeed : Float
maxSpeed =
    -- px/ms
    0.03


doorOpenDuration : Time
doorOpenDuration =
    1.5 * second


peopleTransitionDuration : Time
peopleTransitionDuration =
    3 * second


type ElevatorState
    = Idle
    | DoorsClosing Time
    | PeopleEntering Time
    | DoorsOpening Time
    | Moving ElevatorDirection Time FloorNumber


type ElevatorDirection
    = Up
    | Down


type SpotInElevator
    = SpotInElevator Elevator Int


type Gender
    = Male
    | Female


type PersonState
    = Waiting Int FloorNumber
      -- the index of position in row fron and to which the person is going
    | EnteringElevator Int SpotInElevator
      -- The index of the position where the person is
    | InElevator SpotInElevator
    | LeavingElevator SpotInElevator


type alias Person =
    { -- The floor number the person wants to go to
      target : FloorNumber
    , state : PersonState
    , gender : Gender
    , born : String
    }


type alias People =
    List Person


type alias Floor =
    { number : FloorNumber
    , buttonsPressed : Set ElevatorDirection
    }


type alias Floors =
    List Floor


type alias FloorNumber =
    Int


type alias ElevatorNumber =
    Int


type alias Elevation =
    Float


type alias Elevator =
    { number : ElevatorNumber
    , sourceFloor : FloorNumber
    , buttonsPressed : Floors
    , state : ElevatorState
    }


type alias NextPersonAdded =
    { floor : FloorNumber
    , targetFloor : FloorNumber
    , gender : Gender
    , eta : Time
    }


type GamePhase
    = Running
    | GameOver FloorNumber


type alias Model =
    { floors : Floors
    , elevators : List Elevator
    , people : People
    , time : Time
    , nextComer : NextPersonAdded
    , randomSeed : Random.Seed
    , phase : GamePhase
    }
