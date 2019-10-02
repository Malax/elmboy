module Component.CPU.Condition exposing (Condition(..), check)

import Component.CPU.FlagRegister as FlagRegister


type Condition
    = Always
    | Zero
    | NotZero
    | Carry
    | NotCarry


check : Condition -> Int -> Bool
check condition flags =
    case condition of
        Always ->
            True

        Zero ->
            FlagRegister.getFlag FlagRegister.Zero flags

        NotZero ->
            FlagRegister.getFlag FlagRegister.Zero flags |> not

        Carry ->
            FlagRegister.getFlag FlagRegister.Carry flags

        NotCarry ->
            FlagRegister.getFlag FlagRegister.Carry flags |> not
