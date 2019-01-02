module Component.APU.Channel1 exposing (Channel1, emulate, init, writeNR10, writeNR11, writeNR12, writeNR13, writeNR14)


type alias Channel1 =
    { currentSample : Float }


init : Channel1
init =
    { currentSample = 0 }


emulate : Int -> Channel1 -> Channel1
emulate cycles channel =
    channel


writeNR10 : Int -> Channel1 -> Channel1
writeNR10 value channel =
    channel


writeNR11 : Int -> Channel1 -> Channel1
writeNR11 value channel =
    channel


writeNR12 : Int -> Channel1 -> Channel1
writeNR12 value channel =
    channel


writeNR13 : Int -> Channel1 -> Channel1
writeNR13 value channel =
    channel


writeNR14 : Int -> Channel1 -> Channel1
writeNR14 value channel =
    channel
