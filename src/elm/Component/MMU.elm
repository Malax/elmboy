module Component.MMU exposing
    ( readWord16
    , readWord8
    , readWord8Chunk
    , writeWord16
    , writeWord8
    )

import Array exposing (Array)
import Bitwise
import Component.APU as APU
import Component.CPU as CPU
import Component.Cartridge as Cartridge
import Component.Joypad as Joypad
import Component.PPU as PPU
import Component.RAM as RAM
import Component.Timer as Timer
import GameBoy exposing (GameBoy)
import Types exposing (MemoryAddress)


readWord8 : GameBoy -> MemoryAddress -> Int
readWord8 gameBoy address =
    {-
       This could be solved more elegantly, but as memory access is happening every cycle, we have to implement this in the most
       performant way with as little as possible overhead.
    -}
    if address <= 0x0100 && not gameBoy.bootRomDisabled then
        -- Practically, those values will never be read anymore as the bootrom support was removed during early development to avoid
        -- copyright and trademark issues. But if they are, we just treat the whole boot ROM as a series of NOPs.
        0x00

    else if address <= 0x7FFF then
        -- Cartridge ROM
        Cartridge.readWord8 gameBoy.cartridge address

    else if address >= 0x8000 && address <= 0x9FFF then
        -- VRAM
        PPU.readVRAM gameBoy.ppu 0 (address - 0x8000)

    else if address >= 0xA000 && address <= 0xBFFF then
        -- Cartridge RAM
        Cartridge.readWord8 gameBoy.cartridge address

    else if address >= 0xC000 && address <= 0xCFFF then
        -- Work RAM Bank 0
        RAM.readWord8 gameBoy.workRamBank0 (address - 0xC000)

    else if address >= 0xD000 && address <= 0xDFFF then
        -- Work RAM Bank 1
        RAM.readWord8 gameBoy.workRamBank1 (address - 0xD000)

    else if address >= 0xE000 && address <= 0xEFFF then
        -- Work RAM Bank 0 (Mirror)
        RAM.readWord8 gameBoy.workRamBank0 (address - 0xE000)

    else if address >= 0xF000 && address <= 0xFDFF then
        -- Work RAM Bank 1 (Partial Mirror)
        RAM.readWord8 gameBoy.workRamBank1 (address - 0xF000)

    else if address >= 0xFE00 && address <= 0xFE9F then
        -- OAM
        PPU.readOamRam gameBoy.ppu (address - 0xFE00)

    else if address >= 0xFF80 && address <= 0xFFFE then
        -- HRAM
        RAM.readWord8 gameBoy.hram (address - 0xFF80)

    else if address == 0xFF00 then
        Joypad.readRegister gameBoy.joypad

    else if address == 0xFF04 then
        Timer.readDivider gameBoy.timer

    else if address == 0xFF05 then
        Timer.readTima gameBoy.timer

    else if address == 0xFF06 then
        Timer.readTma gameBoy.timer

    else if address == 0xFF07 then
        Timer.readTac gameBoy.timer

    else if address == 0xFF10 then
        APU.readNR10 gameBoy.apu

    else if address == 0xFF11 then
        APU.readNR11 gameBoy.apu

    else if address == 0xFF12 then
        APU.readNR12 gameBoy.apu

    else if address == 0xFF13 then
        APU.readNR13 gameBoy.apu

    else if address == 0xFF14 then
        APU.readNR14 gameBoy.apu

    else if address == 0xFF16 then
        APU.readNR21 gameBoy.apu

    else if address == 0xFF17 then
        APU.readNR22 gameBoy.apu

    else if address == 0xFF18 then
        APU.readNR23 gameBoy.apu

    else if address == 0xFF19 then
        APU.readNR24 gameBoy.apu

    else if address == 0xFF1A then
        APU.readNR30 gameBoy.apu

    else if address == 0xFF1B then
        APU.readNR31 gameBoy.apu

    else if address == 0xFF1C then
        APU.readNR32 gameBoy.apu

    else if address == 0xFF1D then
        APU.readNR33 gameBoy.apu

    else if address == 0xFF1E then
        APU.readNR34 gameBoy.apu

    else if address == 0xFF20 then
        APU.readNR41 gameBoy.apu

    else if address == 0xFF21 then
        APU.readNR42 gameBoy.apu

    else if address == 0xFF22 then
        APU.readNR43 gameBoy.apu

    else if address == 0xFF23 then
        APU.readNR44 gameBoy.apu

    else if address == 0xFF24 then
        APU.readNR50 gameBoy.apu

    else if address == 0xFF25 then
        APU.readNR51 gameBoy.apu

    else if address == 0xFF26 then
        APU.readNR52 gameBoy.apu

    else if address >= 0xFF30 && address <= 0xFF3F then
        APU.readWaveRam (address - 0xFF30) gameBoy.apu

    else if address == 0xFF40 then
        PPU.readLCDC gameBoy.ppu

    else if address == 0xFF41 then
        PPU.readLCDStatus gameBoy.ppu

    else if address == 0xFF42 then
        PPU.readScrollY gameBoy.ppu

    else if address == 0xFF43 then
        PPU.readScrollX gameBoy.ppu

    else if address == 0xFF44 then
        PPU.readLY gameBoy.ppu

    else if address == 0xFF45 then
        PPU.readLYC gameBoy.ppu

    else if address == 0xFF47 then
        PPU.readBackgroundPalette gameBoy.ppu

    else if address == 0xFF48 then
        PPU.readObjectPalette0 gameBoy.ppu

    else if address == 0xFF49 then
        PPU.readObjectPalette1 gameBoy.ppu

    else if address == 0xFF4A then
        PPU.readWindowY gameBoy.ppu

    else if address == 0xFF4B then
        PPU.readWindowX gameBoy.ppu

    else if address == 0xFF50 then
        if gameBoy.bootRomDisabled then
            0x01

        else
            0x00

    else if address == 0xFF0F then
        gameBoy.cpu.interruptFlag

    else if address == 0xFFFF then
        gameBoy.cpu.interruptEnable

    else
        0xFF


writeWord8 : MemoryAddress -> Int -> GameBoy -> GameBoy
writeWord8 address value gameBoy =
    let
        sanitizedValue =
            Bitwise.and 0xFF value
    in
    if address <= 0x7FFF then
        -- Cartridge ROM, will be intercepted by a possible memory bank controller
        GameBoy.setCartridge (Cartridge.writeWord8 address value gameBoy.cartridge) gameBoy

    else if address >= 0x8000 && address <= 0x9FFF then
        -- VRAM
        GameBoy.setPPU (PPU.writeVRAM 0 (address - 0x8000) sanitizedValue gameBoy.ppu) gameBoy

    else if address >= 0xA000 && address <= 0xBFFF then
        -- Cartridge RAM
        GameBoy.setCartridge (Cartridge.writeWord8 address value gameBoy.cartridge) gameBoy

    else if address >= 0xC000 && address <= 0xCFFF then
        -- Work RAM Bank 0
        GameBoy.setWorkRamBank0 (RAM.writeWord8 (address - 0xC000) sanitizedValue gameBoy.workRamBank0) gameBoy

    else if address >= 0xD000 && address <= 0xDFFF then
        -- Work RAM Bank 1
        GameBoy.setWorkRamBank1 (RAM.writeWord8 (address - 0xD000) sanitizedValue gameBoy.workRamBank1) gameBoy

    else if address >= 0xE000 && address <= 0xEFFF then
        -- Work RAM Bank 0 (Mirror)
        GameBoy.setWorkRamBank0 (RAM.writeWord8 (address - 0xE000) sanitizedValue gameBoy.workRamBank0) gameBoy

    else if address >= 0xF000 && address <= 0xFDFF then
        -- Work RAM Bank 1 (Partial Mirror)
        GameBoy.setWorkRamBank1 (RAM.writeWord8 (address - 0xF000) sanitizedValue gameBoy.workRamBank1) gameBoy

    else if address >= 0xFE00 && address <= 0xFE9F then
        -- OAM
        GameBoy.setPPU (PPU.writeOAMRam (address - 0xFE00) sanitizedValue gameBoy.ppu) gameBoy

    else if address >= 0xFF80 && address <= 0xFFFE then
        -- HRAM
        GameBoy.setHRAM (RAM.writeWord8 (address - 0xFF80) sanitizedValue gameBoy.hram) gameBoy

    else if address == 0xFF00 then
        GameBoy.setJoypad (Joypad.writeRegister sanitizedValue gameBoy.joypad) gameBoy

    else if address == 0xFF04 then
        GameBoy.setTimer (Timer.resetDivider gameBoy.timer) gameBoy

    else if address == 0xFF05 then
        GameBoy.setTimer (Timer.writeTima sanitizedValue gameBoy.timer) gameBoy

    else if address == 0xFF06 then
        GameBoy.setTimer (Timer.writeTma sanitizedValue gameBoy.timer) gameBoy

    else if address == 0xFF07 then
        GameBoy.setTimer (Timer.writeTac sanitizedValue gameBoy.timer) gameBoy

    else if address == 0xFF10 then
        GameBoy.setAPU (APU.writeNR10 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF11 then
        GameBoy.setAPU (APU.writeNR11 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF12 then
        GameBoy.setAPU (APU.writeNR12 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF13 then
        GameBoy.setAPU (APU.writeNR13 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF14 then
        GameBoy.setAPU (APU.writeNR14 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF16 then
        GameBoy.setAPU (APU.writeNR21 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF17 then
        GameBoy.setAPU (APU.writeNR22 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF18 then
        GameBoy.setAPU (APU.writeNR23 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF19 then
        GameBoy.setAPU (APU.writeNR24 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF1A then
        GameBoy.setAPU (APU.writeNR30 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF1B then
        GameBoy.setAPU (APU.writeNR31 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF1C then
        GameBoy.setAPU (APU.writeNR32 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF1D then
        GameBoy.setAPU (APU.writeNR33 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF1E then
        GameBoy.setAPU (APU.writeNR34 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF20 then
        GameBoy.setAPU (APU.writeNR41 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF21 then
        GameBoy.setAPU (APU.writeNR42 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF22 then
        GameBoy.setAPU (APU.writeNR43 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF23 then
        GameBoy.setAPU (APU.writeNR44 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF24 then
        GameBoy.setAPU (APU.writeNR50 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF25 then
        GameBoy.setAPU (APU.writeNR51 sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF26 then
        GameBoy.setAPU (APU.writeNR52 sanitizedValue gameBoy.apu) gameBoy

    else if address >= 0xFF30 && address <= 0xFF3F then
        GameBoy.setAPU (APU.writeWaveRam (address - 0xFF30) sanitizedValue gameBoy.apu) gameBoy

    else if address == 0xFF40 then
        GameBoy.setPPU (PPU.writeLCDC sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF41 then
        GameBoy.setPPU (PPU.writeLCDStatus sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF42 then
        GameBoy.setPPU (PPU.writeScrollY sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF43 then
        GameBoy.setPPU (PPU.writeScrollX sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF44 then
        GameBoy.setPPU (PPU.writeLY sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF45 then
        GameBoy.setPPU (PPU.writeLYC sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF46 then
        oamDMATransfer sanitizedValue gameBoy

    else if address == 0xFF47 then
        GameBoy.setPPU (PPU.writeBackgroundPalette sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF48 then
        GameBoy.setPPU (PPU.writeObjectPalette0 sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF49 then
        GameBoy.setPPU (PPU.writeObjectPalette1 sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF4A then
        GameBoy.setPPU (PPU.writeWindowY sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF4B then
        GameBoy.setPPU (PPU.writeWindowX sanitizedValue gameBoy.ppu) gameBoy

    else if address == 0xFF50 then
        { gameBoy | bootRomDisabled = True }

    else if address == 0xFF68 then
        let
            _ =
                ()
        in
        gameBoy

    else if address == 0xFF69 then
        let
            _ =
                ()
        in
        gameBoy

    else if address == 0xFF0F then
        GameBoy.setCPU (CPU.setInterruptFlag sanitizedValue gameBoy.cpu) gameBoy

    else if address == 0xFFFF then
        GameBoy.setCPU (CPU.setInterruptEnable sanitizedValue gameBoy.cpu) gameBoy

    else
        gameBoy


readWord16 : GameBoy -> MemoryAddress -> Int
readWord16 gameBoy address =
    let
        highByte =
            readWord8 gameBoy (address + 1)

        lowByte =
            readWord8 gameBoy address
    in
    Bitwise.or (Bitwise.shiftLeftBy 8 highByte) lowByte


writeWord16 : MemoryAddress -> Int -> GameBoy -> GameBoy
writeWord16 address value gameBoy =
    let
        highByte =
            Bitwise.and 0xFF00 value |> Bitwise.shiftRightZfBy 8

        lowByte =
            Bitwise.and 0xFF value
    in
    gameBoy
        |> writeWord8 address lowByte
        |> writeWord8 (address + 1) highByte


readWord8Chunk : GameBoy -> MemoryAddress -> Int -> Array Int
readWord8Chunk gameBoy startAddress length =
    -- This function is often used for reading a chunk from work RAM 0 or 1. We're special-casing those address ranges here for performance reasons.
    if startAddress >= 0xC000 && startAddress + length <= 0xCFFF then
        RAM.readWord8Slice gameBoy.workRamBank0 (startAddress - 0xC000) length

    else if startAddress >= 0xD000 && startAddress + length <= 0xDFFF then
        RAM.readWord8Slice gameBoy.workRamBank1 (startAddress - 0xD000) length

    else
        -- TODO: This seems like a good candidate for a performance improvement
        List.repeat length startAddress
            |> List.indexedMap (+)
            |> List.map (readWord8 gameBoy)
            |> Array.fromList



-- Helpers


oamDMATransfer : Int -> GameBoy -> GameBoy
oamDMATransfer byte gameBoy =
    let
        chunk =
            readWord8Chunk gameBoy (Bitwise.shiftLeftBy 8 byte) (40 * 4)
    in
    GameBoy.setPPU (PPU.replaceOAMRam chunk gameBoy.ppu) gameBoy
