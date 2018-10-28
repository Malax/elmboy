module Gamepad
    exposing
        ( Analog(..)
        , Digital(..)
        , Gamepad
        , digitalToString
        , dpadPosition
        , getIndex
        , isPressed
        , leftStickPosition
        , rightStickPosition
        , value
        , wasClicked
        , wasReleased
        )

{-| First, you need to get a list of `Gamepad` objects either
Gamepad.Simple](TODO) or [Gamepad.Advanced](TODO); once you have the
`Gamepad` object, you use the functions in this module to figure out
the buttons and axis that the user is activating.

The `Digital` and `Analog` types define names for the controls available on gamepads, and
can be used with various getter functions, for example

    Gamepad.value LeftTriggerAnalog gamepad == 0.1


# Gamepads

@docs Gamepad, getIndex


# Digital input

@docs Digital, isPressed, wasClicked, wasReleased, dpadPosition


# Analog input

@docs Analog, value, leftStickPosition, rightStickPosition


# Stuff you don't need

@docs digitalToString

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Gamepad.Private as Private
    exposing
        ( GamepadFrame
        , Mapping
        , Origin(..)
        , OriginType(..)
        , boolToNumber
        )
import List.Extra
import Time


-- API ------------------------------------------------------------------------


{-| A gamepad with a known mapping.
You can use all control getters to query its state.
-}
type alias Gamepad =
    Private.Gamepad



-- API ------------------------------------------------------------------------


{-| This type defines names for all available digital controls, whose
state can be either `True` or `False`.

Since the names cover all controls defined the [W3C draft](https://www.w3.org/TR/gamepad/),
they are used also when when remapping the gamepad, to declare which action should be
bound to which control.

-}
type Digital
    = A
    | B
    | X
    | Y
    | Start
    | Back
    | Home
    | LeftStickPress
    | LeftStickUp
    | LeftStickDown
    | LeftStickLeft
    | LeftStickRight
    | LeftTrigger
    | LeftBumper
    | RightStickPress
    | RightStickUp
    | RightStickDown
    | RightStickLeft
    | RightStickRight
    | RightTrigger
    | RightBumper
    | DpadUp
    | DpadDown
    | DpadLeft
    | DpadRight



-- API ------------------------------------------------------------------------


{-| Some controls can be accessed also as analog and this type defines special
names for them.
-}
type Analog
    = LeftX
    | LeftY
    | LeftTriggerAnalog
    | RightX
    | RightY
    | RightTriggerAnalog


type OneOrTwo a
    = One a
    | Two a a


analogToDestination : Analog -> OneOrTwo Digital
analogToDestination analog =
    case analog of
        LeftX ->
            Two LeftStickLeft LeftStickRight

        LeftY ->
            Two LeftStickDown LeftStickUp

        LeftTriggerAnalog ->
            One LeftTrigger

        RightX ->
            Two RightStickLeft RightStickRight

        RightY ->
            Two RightStickDown RightStickUp

        RightTriggerAnalog ->
            One RightTrigger


{-| This function is mostly used internally, you probably don't need it.
-}
digitalToString : Digital -> String
digitalToString destination =
    case destination of
        A ->
            "A"

        B ->
            "B"

        X ->
            "X"

        Y ->
            "Y"

        Start ->
            "Start"

        Back ->
            "Back"

        Home ->
            "Home"

        LeftStickLeft ->
            "LeftLeft"

        LeftStickRight ->
            "LeftRight"

        LeftStickUp ->
            "LeftUp"

        LeftStickDown ->
            "LeftDown"

        LeftStickPress ->
            "LeftPress"

        LeftBumper ->
            "LeftBumper"

        LeftTrigger ->
            "LeftTrigger"

        RightStickLeft ->
            "RightLeft"

        RightStickRight ->
            "RightRight"

        RightStickUp ->
            "RightUp"

        RightStickDown ->
            "RightDown"

        RightStickPress ->
            "RightPress"

        RightBumper ->
            "RightBumper"

        RightTrigger ->
            "RightTrigger"

        DpadUp ->
            "DpadUp"

        DpadDown ->
            "DpadDown"

        DpadLeft ->
            "DpadLeft"

        DpadRight ->
            "DpadRight"



-- API -----------------------------------------------------------------------


{-| Get the index of a gamepad.
To match the LED indicator on XBOX gamepads, indices start from 1.

    getIndex gamepad == 2

-}
getIndex : Gamepad -> Int
getIndex (Private.Gamepad mapping currentFrame previousFrame) =
    currentFrame.index



-- API -----------------------------------------------------------------------


{-| X and Y coordinates (-1 to +1) of the left stick.
-}
leftStickPosition : Gamepad -> { x : Float, y : Float }
leftStickPosition pad =
    { x = value pad LeftX
    , y = value pad LeftY
    }



-- API -----------------------------------------------------------------------


{-| X and Y coordinates (-1 to +1) of the right stick.
-}
rightStickPosition : Gamepad -> { x : Float, y : Float }
rightStickPosition pad =
    { x = value pad RightX
    , y = value pad RightY
    }



-- API -----------------------------------------------------------------------


{-| X and Y coordinates (-1, 0 or +1) of the digital pad.
-}
dpadPosition : Gamepad -> { x : Int, y : Int }
dpadPosition pad =
    let
        toInt d =
            boolToNumber (isPressed pad d)
    in
    { x = toInt DpadRight - toInt DpadLeft
    , y = toInt DpadUp - toInt DpadDown
    }



-- API -----------------------------------------------------------------------


{-| Returns `True` if the button is currently being held down.
-}
isPressed : Gamepad -> Digital -> Bool
isPressed (Private.Gamepad mapping currentFrame previousFrame) digital =
    getAsBool digital mapping currentFrame



-- API -----------------------------------------------------------------------


{-| Returns `True` when a button **changes** from being held down to going back up.
-}
wasReleased : Gamepad -> Digital -> Bool
wasReleased (Private.Gamepad mapping currentFrame previousFrame) digital =
    getAsBool digital mapping previousFrame && not (getAsBool digital mapping currentFrame)



-- API -----------------------------------------------------------------------


{-| Returns `True` when a button **changes** from being up to being held down.
-}
wasClicked : Gamepad -> Digital -> Bool
wasClicked (Private.Gamepad mapping currentFrame previousFrame) digital =
    not (getAsBool digital mapping previousFrame) && getAsBool digital mapping currentFrame



-- API -----------------------------------------------------------------------


{-| Returns a single value from an analog control.
-}
value : Gamepad -> Analog -> Float
value (Private.Gamepad mapping currentFrame previousFrame) analog =
    case analogToDestination analog of
        One positive ->
            getAsFloat positive mapping currentFrame

        Two negative positive ->
            getAxis negative positive mapping currentFrame


mappingToOrigin : Digital -> Mapping -> Maybe Origin
mappingToOrigin destination mapping =
    Dict.get (digitalToString destination) mapping


axisToButton : Float -> Bool
axisToButton n =
    n > 0.6


buttonToAxis : Bool -> Float
buttonToAxis =
    boolToNumber


reverseAxis : Bool -> Float -> Float
reverseAxis isReverse n =
    if isReverse then
        -n
    else
        n


getAsBool : Digital -> Mapping -> GamepadFrame -> Bool
getAsBool destination mapping frame =
    case mappingToOrigin destination mapping of
        Nothing ->
            False

        Just (Origin isReverse Axis index) ->
            Array.get index frame.axes
                |> Maybe.withDefault 0
                |> reverseAxis isReverse
                |> axisToButton

        Just (Origin isReverse Button index) ->
            Array.get index frame.buttons
                |> Maybe.map Tuple.first
                |> Maybe.withDefault False


getAsFloat : Digital -> Mapping -> GamepadFrame -> Float
getAsFloat destination mapping frame =
    case mappingToOrigin destination mapping of
        Nothing ->
            0

        Just (Origin isReverse Axis index) ->
            Array.get index frame.axes
                |> Maybe.withDefault 0
                |> reverseAxis isReverse

        Just (Origin isReverse Button index) ->
            Array.get index frame.buttons
                |> Maybe.map Tuple.second
                |> Maybe.withDefault 0


getAxis : Digital -> Digital -> Mapping -> GamepadFrame -> Float
getAxis negativeDestination positiveDestination mapping frame =
    let
        negative =
            getAsFloat negativeDestination mapping frame

        positive =
            getAsFloat positiveDestination mapping frame
    in
    -- if both point to the same Origin, we need just one
    if positive == -negative then
        positive
    else
        positive - negative
