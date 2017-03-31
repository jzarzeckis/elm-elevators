module App exposing (..)

import Html exposing (Html, Attribute, text, div, span, i)
import Html.Attributes exposing (class, style)
import Html.Lazy exposing (lazy, lazy2, lazy3)
import AnimationFrame
import Time exposing (Time, millisecond, second)
import List exposing (range)
import Set exposing (Set)


floorHeight : Int
floorHeight =
    50


elevatorWidth : Int
elevatorWidth =
    40


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


elevatorY : Time -> Elevator -> Int
elevatorY time elevator =
    case elevator.state of
        Moving _ eta floor ->
            Debug.crash "Acceleration formulas haven't been implemented yet"

        _ ->
            floorHeight * elevator.sourceFloor


hs =
    ( "height", (toString floorHeight) ++ "px" )


ws =
    ( "width", (toString elevatorWidth) ++ "px" )


heightStyle : Attribute Msg
heightStyle =
    style [ hs ]


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
    = Waiting FloorNumber
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
    Int


type alias Elevator =
    { number : ElevatorNumber
    , sourceFloor : FloorNumber
    , buttonsPressed : Floors
    , state : ElevatorState
    }


type alias Model =
    { floors : Floors
    , elevators : List Elevator
    , people : People
    , time : Time
    }


init : ( Model, Cmd Msg )
init =
    let
        floors =
            range 0 4
                |> List.map (\f -> Floor f Set.empty)
    in
        ( Model
            floors
            (range 0 4
                |> List.map (\e -> Elevator e ((e - 1) % (List.length floors)) [] Idle)
            )
            (floors
                |> List.map
                    (\f -> Person ((f.number - 1) % List.length floors) (Waiting f.number) Female)
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
            ( { model | time = model.time + time }
            , Cmd.none
            )


elevatorTransforms : ElevatorNumber -> Elevation -> Int -> Attribute Msg
elevatorTransforms nr elevation wh =
    style
        [ ws
        , ( "transform"
          , "translate3d("
                ++ (toString <| nr * elevatorWidth * 3 // 2 + 200)
                ++ "px, "
                ++ (toString <| wh - elevation - floorHeight)
                ++ "px, 0px)"
          )
        ]


renderButtonsPressed : Floors -> Floors -> Html Msg
renderButtonsPressed =
    lazy2
        (\floors pressed ->
            floors
                |> List.map
                    (\f ->
                        let
                            c : String
                            c =
                                if List.member f pressed then
                                    " activated"
                                else
                                    ""
                        in
                            span [ class <| "buttonpress" ++ c ] [ text <| toString f.number ]
                    )
                |> span [ class "buttonindicator" ]
        )


elevatorDoors : Int -> Html Msg
elevatorDoors =
    lazy
        (\offset ->
            div
                [ class "doors"
                , style
                    [ ( "transform"
                      , "translate3d("
                            ++ toString offset
                            ++ "px, 0px, 0px)"
                      )
                    ]
                ]
                []
        )


doorOffset : Time -> Elevator -> Int
doorOffset time el =
    case el.state of
        DoorsOpening eta ->
            (toFloat elevatorWidth)
                * (eta - time)
                / doorOpenDuration
                |> round

        DoorsClosing eta ->
            (toFloat elevatorWidth)
                * (1 - (eta - time) / doorOpenDuration)
                |> round

        PeopleEntering _ ->
            elevatorWidth

        _ ->
            0


renderElevator : Floors -> Time -> Int -> Elevator -> Html Msg
renderElevator floors time wh el =
    lazyElevator
        ( doorOffset time el, elevatorY time el, wh )
        floors
        el


shownFloorNumber : Elevation -> String
shownFloorNumber elev =
    elev
        // floorHeight
        |> toString


lazyElevator : ( Int, Elevation, Int ) -> Floors -> Elevator -> Html Msg
lazyElevator =
    lazy3
        (\( doorOffset, elevation, wh ) floors e ->
            div [ class "elevator movable", elevatorTransforms e.number elevation wh ]
                [ span [ class "directionindicator directionindicatorup" ] [ i [ class "fa fa-arrow-circle-up up activated" ] [] ]
                , span [ class "floorindicator" ] [ text <| shownFloorNumber elevation ]
                , span [ class "directionindicator directionindicatordown" ] [ i [ class "fa fa-arrow-circle-down down activated" ] [] ]
                , renderButtonsPressed floors e.buttonsPressed
                , elevatorDoors doorOffset
                ]
        )


renderElevators : Floors -> Time -> List Elevator -> Int -> Html Msg
renderElevators floors time elevators wh =
    elevators
        |> List.map (renderElevator floors time wh)
        |> div [ class "elevators" ]


renderFloor : Int -> Floor -> Html Msg
renderFloor floorCount floor =
    div [ class "floor", style [ ( "top", toString ((floorCount - floor.number - 1) * floorHeight) ++ "px" ) ] ]
        [ span [ class "floornumber" ] [ text <| toString floor.number ]
        , span [ class "buttonindicator" ]
            [ i [ class "fa fa-arrow-circle-up up" ] []
            , text " "
            , i [ class "fa fa-arrow-circle-down down" ] []
            ]
        ]


renderFloors : Floors -> Html Msg
renderFloors =
    lazy
        (\f ->
            f
                |> List.map (renderFloor (List.length f))
                |> div [ class "floors" ]
        )


personPosition : Time -> List Elevator -> Person -> ( Int, Elevation )
personPosition _ _ _ =
    ( 10, 10 )


lazyPerson : ( Int, Elevation ) -> Gender -> Html Msg
lazyPerson =
    lazy2
        (\( x, y ) gender ->
            let
                g =
                    case gender of
                        Male ->
                            "male"

                        Female ->
                            "female"
            in
                i
                    [ class <| "movable fa user fa-" ++ g
                    , style
                        [ ( "transform"
                          , "translate3d("
                                ++ toString x
                                ++ "px, "
                                ++ toString y
                                ++ "px, 0px)"
                          )
                        ]
                    ]
                    []
        )


renderPerson : Time -> List Elevator -> Person -> Html Msg
renderPerson time elevators person =
    lazyPerson
        (personPosition time elevators person)
        person.gender


renderPeople : Time -> List Elevator -> People -> Html Msg
renderPeople time elevators people =
    people
        |> List.map (renderPerson time elevators)
        |> div [ class "people" ]


worldAttributes : Int -> List (Attribute Msg)
worldAttributes height =
    [ class "innerworld", style [ ( "height", toString height ++ "px" ) ] ]


view : Model -> Html Msg
view model =
    let
        totalHeight =
            List.length model.floors * floorHeight
    in
        div [ class "container" ]
            [ div [ class "world" ]
                [ div (worldAttributes totalHeight)
                    [ renderFloors model.floors
                    , renderElevators model.floors model.time model.elevators totalHeight
                    , renderPeople model.time model.elevators model.people
                    ]
                ]
              -- , div [ class "timer" ] [ text <| toString model.time ]
            ]


isOnFloor : FloorNumber -> Person -> Bool
isOnFloor floor person =
    case person.state of
        Waiting nr ->
            nr == floor

        _ ->
            False


isButtonPushed : (Int -> Int -> Bool) -> Floor -> People -> Bool
isButtonPushed cmp floor people =
    people
        |> List.filter (isOnFloor floor.number)
        |> List.map .target
        |> List.any (cmp floor.number)


upPushed : Floor -> People -> Bool
upPushed =
    isButtonPushed (<)


downPushed : Floor -> People -> Bool
downPushed =
    isButtonPushed (>)


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick
