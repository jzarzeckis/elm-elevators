module App exposing (..)

import Html exposing (Html, Attribute, text, div, span, i)
import Html.Attributes exposing (class, style)
import Html.Lazy exposing (lazy)
import AnimationFrame exposing (times)
import Time exposing (Time)
import List exposing (range)


floorHeight : Int
floorHeight =
    50


elevatorWidth : Int
elevatorWidth =
    40


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
renderPerson =
    always <| div [ class "pers" ] [ text "😀" ]


elevatorStyle : ElevatorNumber -> FloorNumber -> Attribute Msg
elevatorStyle nr elev =
    style
        [ ws
        , ( "transform"
          , "translate3d("
                ++ (toString <| nr * elevatorWidth * 3 // 2 + 200)
                ++ "px, "
                ++ (toString <| elev * floorHeight)
                ++ "px, 0px)"
          )
        ]


renderElevator : Elevator -> Html Msg
renderElevator e =
    div [ class "elevator movable", elevatorStyle e.number e.sourceFloor ] []


renderElevators : Model -> Html Msg
renderElevators =
    .elevators
        >> lazy
            (div [ class "elevators" ]
                << List.map renderElevator
            )


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


renderFloors : Model -> Html Msg
renderFloors =
    .floors
        >> lazy
            ((\f ->
                List.map (renderFloor (List.length f)) f
             )
                >> div [ class "floors" ]
            )


worldAttributes : Int -> List (Attribute Msg)
worldAttributes floorCount =
    [ class "innerworld", style [ ( "height", toString (floorHeight * floorCount) ++ "px" ) ] ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "world" ]
            [ div (worldAttributes <| List.length model.floors)
                [ renderFloors model
                , renderElevators model
                ]
            ]
        ]


upPushed : Floor -> Bool
upPushed floor =
    List.any ((<) floor.number) floor.people


downPushed : Floor -> Bool
downPushed floor =
    List.any ((>) floor.number) floor.people



-- floorButtonsPushed : Floor -> Floors -> List ( FloorNumber, Bool )
-- floorButtonsPushed floor floors =
--     List.map (\f -> ( f.number, List.member f floor.people )) floors


subscriptions : Model -> Sub Msg
subscriptions model =
    times Tick
