module Component.CPU exposing
    ( CPU
    , init
    , readRegisterA
    , readRegisterAF
    , readRegisterB
    , readRegisterBC
    , readRegisterC
    , readRegisterD
    , readRegisterDE
    , readRegisterE
    , readRegisterF
    , readRegisterH
    , readRegisterHL
    , readRegisterL
    , readRegisterPC
    , readRegisterSP
    , setHalted
    , setInterruptData
    , setInterruptEnable
    , setInterruptFlag
    , setInterruptMasterEnable
    , writeRegisterA
    , writeRegisterAF
    , writeRegisterB
    , writeRegisterBC
    , writeRegisterC
    , writeRegisterD
    , writeRegisterDE
    , writeRegisterE
    , writeRegisterF
    , writeRegisterH
    , writeRegisterHL
    , writeRegisterL
    , writeRegisterPC
    , writeRegisterSP
    )

import Bitwise


type alias CPU =
    { af : Int
    , bc : Int
    , de : Int
    , hl : Int
    , pc : Int
    , sp : Int
    , halted : Bool
    , interruptFlag : Int
    , interruptEnable : Int
    , interruptMasterEnable : Bool
    }


init : CPU
init =
    { af = 0x01B0
    , bc = 0x13
    , de = 0xD8
    , hl = 0x014D
    , pc = 0x0100
    , sp = 0xFFFE
    , halted = False
    , interruptFlag = 0xE1
    , interruptEnable = 0x00
    , interruptMasterEnable = True
    }


readRegisterA : CPU -> Int
readRegisterA cpu =
    Bitwise.shiftRightZfBy 8 cpu.af


readRegisterB : CPU -> Int
readRegisterB cpu =
    Bitwise.shiftRightZfBy 8 cpu.bc


readRegisterC : CPU -> Int
readRegisterC cpu =
    Bitwise.and 0xFF cpu.bc


readRegisterD : CPU -> Int
readRegisterD cpu =
    Bitwise.shiftRightZfBy 8 cpu.de


readRegisterE : CPU -> Int
readRegisterE cpu =
    Bitwise.and 0xFF cpu.de


readRegisterF : CPU -> Int
readRegisterF cpu =
    Bitwise.and 0xFF cpu.af


readRegisterH : CPU -> Int
readRegisterH cpu =
    Bitwise.shiftRightZfBy 8 cpu.hl


readRegisterL : CPU -> Int
readRegisterL cpu =
    Bitwise.and 0xFF cpu.hl


readRegisterAF : CPU -> Int
readRegisterAF =
    .af


readRegisterBC : CPU -> Int
readRegisterBC =
    .bc


readRegisterDE : CPU -> Int
readRegisterDE =
    .de


readRegisterHL : CPU -> Int
readRegisterHL =
    .hl


readRegisterPC : CPU -> Int
readRegisterPC =
    .pc


readRegisterSP : CPU -> Int
readRegisterSP =
    .sp


writeRegisterA : Int -> CPU -> CPU
writeRegisterA value cpu =
    { af = Bitwise.and 0xFF cpu.af |> Bitwise.or (Bitwise.shiftLeftBy 8 value)
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterB : Int -> CPU -> CPU
writeRegisterB value cpu =
    { af = cpu.af
    , bc = Bitwise.and 0xFF cpu.bc |> Bitwise.or (Bitwise.shiftLeftBy 8 value)
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterC : Int -> CPU -> CPU
writeRegisterC value cpu =
    { af = cpu.af
    , bc = Bitwise.and 0xFF00 cpu.bc |> Bitwise.or value
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterD : Int -> CPU -> CPU
writeRegisterD value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = Bitwise.and 0xFF cpu.de |> Bitwise.or (Bitwise.shiftLeftBy 8 value)
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterE : Int -> CPU -> CPU
writeRegisterE value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = Bitwise.and 0xFF00 cpu.de |> Bitwise.or value
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterF : Int -> CPU -> CPU
writeRegisterF value cpu =
    { af = Bitwise.and 0xFF00 cpu.af |> Bitwise.or value
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterH : Int -> CPU -> CPU
writeRegisterH value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = Bitwise.and 0xFF cpu.hl |> Bitwise.or (Bitwise.shiftLeftBy 8 value)
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterL : Int -> CPU -> CPU
writeRegisterL value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = Bitwise.and 0xFF00 cpu.hl |> Bitwise.or value
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterAF : Int -> CPU -> CPU
writeRegisterAF value cpu =
    { af = Bitwise.and 0xFFF0 value -- The lowest 4 bits are always discarded for the F register as per spec
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterBC : Int -> CPU -> CPU
writeRegisterBC value cpu =
    { af = cpu.af
    , bc = Bitwise.and 0xFFFF value
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterDE : Int -> CPU -> CPU
writeRegisterDE value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = Bitwise.and 0xFFFF value
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterHL : Int -> CPU -> CPU
writeRegisterHL value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = Bitwise.and 0xFFFF value
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterPC : Int -> CPU -> CPU
writeRegisterPC value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = Bitwise.and 0xFFFF value
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


writeRegisterSP : Int -> CPU -> CPU
writeRegisterSP value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = Bitwise.and 0xFFFF value
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


setInterruptFlag : Int -> CPU -> CPU
setInterruptFlag value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = value
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


setInterruptEnable : Int -> CPU -> CPU
setInterruptEnable value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = value
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


setInterruptMasterEnable : Bool -> CPU -> CPU
setInterruptMasterEnable value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = cpu.halted
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = value
    }


setHalted : Bool -> CPU -> CPU
setHalted value cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = value
    , interruptFlag = cpu.interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = cpu.interruptMasterEnable
    }


setInterruptData : Bool -> Int -> Bool -> CPU -> CPU
setInterruptData interruptMasterEnable interruptFlag halted cpu =
    { af = cpu.af
    , bc = cpu.bc
    , de = cpu.de
    , hl = cpu.hl
    , pc = cpu.pc
    , sp = cpu.sp
    , halted = halted
    , interruptFlag = interruptFlag
    , interruptEnable = cpu.interruptEnable
    , interruptMasterEnable = interruptMasterEnable
    }
