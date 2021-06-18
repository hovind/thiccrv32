module Clash.RISCV.Decode where

import Clash.Prelude
import Clash.RISCV
import Clash.RISCV.Asm
import Data.Either

decode :: BitVector 32 -> Instr
decode instr =
  case opCode of
    0b0010011 ->
      addi (bv2i $ slice d11 d7 instr)
           (bv2i $ slice d19 d15 instr)
           (slice d31 d20 instr)
    _ ->
      errorX "Could not decode"
  where
    opCode = slice d6 d0 instr

encode :: Instr -> BitVector 32
encode (Imm op rd rs imm) =
    imm ++# pack rs ++# pack op ++# pack rd ++# (0b0010011 :: BitVector 7)
