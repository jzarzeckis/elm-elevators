module App exposing (..)

import Html exposing (Html, Attribute, text, div)
import Html.Attributes exposing (class, style)
import AnimationFrame exposing (times)
import Time exposing (Time)
import List exposing (range)


floorHeight : Int
floorHeight =
    40


elevatorWidth : Int
elevatorWidth =
    20


hs =
    ( "height", (toString floorHeight) ++ "px" )


ws =
    ( "width", (toString elevatorWidth) ++ "px" )


heightStyle : Attribute Msg
heightStyle =
    style [ hs ]


elevatorStyle : ElevatorNumber -> Elevation -> Attribute Msg
elevatorStyle nr elev =
    style
        [ hs
        , ws
        , ( "transform", "translate3d(" ++ (toString <| (toFloat nr) * (toFloat elevatorWidth) * 1.5) ++ "px, " ++ (toString <| (round elev) * floorHeight) ++ "px, 0px)" )
        ]


type ElevatorState
    = Idle
    | DoorsClosing Time
    | DoorsOpening Time
    | PeopleEntering Time
    | MovingUp Time FloorNumber
    | MovingDown Time FloorNumber


type alias Elevation =
    Float


type alias Person =
    -- The floor number the person wants to go to
    FloorNumber


type alias People =
    List Person


type alias Floor =
    { number : FloorNumber
    , people : People
    }


type alias Floors =
    List Floor


type alias FloorNumber =
    Int


type alias ElevatorNumber =
    Int


type alias Elevator =
    { number : ElevatorNumber
    , payload : People
    , sourceFloor : FloorNumber
    , state : ElevatorState
    }


type alias Model =
    { floors : Floors
    , elevators : List Elevator
    , time : Time
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (range 0 6
            |> List.map (\f -> Floor f [ (f + 1) % 4, (f + 2) % 4 ])
        )
        (range 0 4
            |> List.map (\e -> Elevator e [ (e - 1) % 4 ] 0 Idle)
        )
        0
    , Cmd.none
    )


type Msg
    = Tick Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | time = time }
            , Cmd.none
            )


renderPerson : Person -> Html Msg
renderPerson pers =
    div [ class "pers" ] [ text "😀" ]


renderFloor : Floor -> Html Msg
renderFloor floor =
    div [ class "floor", heightStyle ]
        [ div [ class "number" ] [ text <| toString floor.number ]
        , div [ class "ppl" ] (List.map renderPerson floor.people)
        ]


renderElevator : Elevator -> Html Msg
renderElevator e =
    div [ class "elevator", elevatorStyle e.number (toFloat e.sourceFloor) ] []


renderFloors : Floors -> List (Html Msg)
renderFloors =
    List.reverse >> List.map renderFloor


view : Model -> Html Msg
view model =
    div [ class "rootContainer" ]
        ((div [ class "floors" ] (renderFloors model.floors))
            :: (div [ class "elevators" ] (List.map renderElevator model.elevators))
            :: []
        )


upPushed : Floor -> Bool
upPushed floor =
    List.any (\p -> p > floor.number) floor.people


downPushed : Floor -> Bool
downPushed floor =
    List.any (\p -> p < floor.number) floor.people



-- floorButtonsPushed : Floor -> Floors -> List ( FloorNumber, Bool )
-- floorButtonsPushed floor floors =
--     List.map (\f -> ( f.number, List.member f floor.people )) floors


subscriptions : Model -> Sub Msg
subscriptions model =
    times Tick
