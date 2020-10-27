{-# LANGUAGE DataKinds, DuplicateRecordFields, NoImplicitPrelude #-}

module Clash.RISCV where

import Clash.Prelude

type XLen = 32 -- Register width
type NReg = 32 -- Number of registers

type Word = BitVector 32
type Value = BitVector XLen
type Register = Index NReg

type Addr = Unsigned 32

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
    | AndI
    | OrI
    | XOrI
    | SLLI
    | SRLI
    | SRAI

data Width
    = Word
    | HalfWord !Sign
    | Byte !Sign

data Sign
    = Signed
    | Unsigned

data Branch
    = BEq
    | BNE
    | BLT !Sign
    | BGE !Sign

