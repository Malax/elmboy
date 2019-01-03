module Component.APU.Timer exposing (Timer, init, reset, update)


type alias Timer =
    { value : Int
    , startValue : Int
    }


init : Int -> Timer
init startValue =
    { value = startValue
    , startValue = startValue
    }


update : Int -> Timer -> ( Timer, Bool )
update clocks timer =
    let
        decrementedValue =
            timer.value - clocks

        underflowOccured =
            decrementedValue <= 0

        updatedValue =
            if underflowOccured then
                timer.startValue - abs decrementedValue

            else
                decrementedValue

        updatedTimer =
            { value = updatedValue
            , startValue = timer.startValue
            }
    in
    ( updatedTimer, underflowOccured )


reset : Timer -> Timer
reset timer =
    init timer.startValue
