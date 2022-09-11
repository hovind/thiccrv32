module Clash.RISCV where

import Clash.Prelude

type XLen = 32 -- Register width
type NReg = 32 -- Number of registers

type Value = BitVector 32
type Register = Index 32

type Addr = Unsigned 32
type PhysAddr = Unsigned 12
type Memory = Vec (2 ^ (BitSize PhysAddr)) Value

data Instr
    = Reg !Op !Register !Register !Register
    -- Register?
    | Imm !OpI !Register !Register !(BitVector 12)
    -- Immediate?
    | Load !Width !Register !Register !(BitVector 12)
    -- LUI (load upper immediate) is used to build 32-bit constants and uses the U-type format
    | Store !Width !Register !Register !(BitVector 12)
    -- Loads are encoded in the I-type format and stores are S-type
    | Branch !Branch !Register !Register !(BitVector 12)
    -- All branch instructions use the B-type instruction format
    | Jump !JumpInstr
    -- The jump and link (JAL) instruction uses the J-type format

data JumpInstr
    = JAL !Register !(BitVector 20)
    | JALR !Register !Register !(BitVector 12)

data Op
    = Add
    | Sub
    | SLT !Sign
    | And
    | Or
    | XOr
    | SLL
    | SRL
    | SRA

data OpI
    = AddI
    | SLTI !Sign
    | XOrI
    | OrI
    | AndI
    | SLLI
    | SRLI
    | SRAI

instance BitPack OpI where
    type BitSize OpI = 3
    pack AddI            = 0b000
    pack (SLTI Signed)   = 0b010
    pack (SLTI Unsigned) = 0b010
    pack XOrI            = 0b100
    pack OrI             = 0b110
    pack AndI            = 0b111
    pack SLLI            = 0b001
    pack SRLI            = 0b101
    pack SRAI            = 0b101

    unpack 0b000 = AddI
    unpack 0b010 = SLTI Signed
    unpack 0b010 = SLTI Unsigned
    unpack 0b100 = XOrI
    unpack 0b110 = OrI
    unpack 0b111 = AndI
    unpack 0b001 = SLLI
    unpack 0b101 = SRLI
    unpack 0b101 = SRAI

data Width
    = Word
    | HalfWord !Sign
    | Byte !Sign
    deriving (Generic, NFDataX)

data Sign
    = Signed
    | Unsigned
    deriving (Generic, NFDataX)

data Branch
    = BEq
    | BNE
    | BLT !Sign
    | BGE !Sign

instance BitPack Branch where
    type BitSize Branch = 3
    pack BEq            = 0b000
    pack BNE            = 0b001
    pack (BLT Signed)   = 0b100
    pack (BGE Signed)   = 0b101
    pack (BLT Unsigned) = 0b110
    pack (BGE Unsigned) = 0b111

    unpack 0b000 = BEq
    unpack 0b001 = BNE
    unpack 0b100 = BLT Signed
    unpack 0b101 = BGE Signed
    unpack 0b110 = BLT Unsigned
    unpack 0b111 = BGE Unsigned
    unpack     _ = errorX "Not a valid branch function"
