module Component.APU.Channel3 exposing (Channel3, emulate, init, writeNR30, writeNR31, writeNR32, writeNR33, writeNR34)


type alias Channel3 =
    { currentSample : Float }


init : Channel3
init =
    { currentSample = 0 }


emulate : Int -> Channel3 -> Channel3
emulate cycles channel =
    channel


writeNR30 : Int -> Channel3 -> Channel3
writeNR30 value channel =
    channel


writeNR31 : Int -> Channel3 -> Channel3
writeNR31 value channel =
    channel


writeNR32 : Int -> Channel3 -> Channel3
writeNR32 value channel =
    channel


writeNR33 : Int -> Channel3 -> Channel3
writeNR33 value channel =
    channel


writeNR34 : Int -> Channel3 -> Channel3
writeNR34 value channel =
    channel
