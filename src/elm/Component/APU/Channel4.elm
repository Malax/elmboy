module Component.APU.Channel4 exposing (Channel4, emulate, init, writeNR41, writeNR42, writeNR43, writeNR44)


type alias Channel4 =
    { currentSample : Float }


init : Channel4
init =
    { currentSample = 0 }


emulate : Int -> Channel4 -> Channel4
emulate cycles channel =
    channel


writeNR41 : Int -> Channel4 -> Channel4
writeNR41 value channel =
    channel


writeNR42 : Int -> Channel4 -> Channel4
writeNR42 value channel =
    channel


writeNR43 : Int -> Channel4 -> Channel4
writeNR43 value channel =
    channel


writeNR44 : Int -> Channel4 -> Channel4
writeNR44 value channel =
    channel
