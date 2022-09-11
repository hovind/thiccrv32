module Clash.RISCV.Decode where

import Clash.Prelude
import Clash.RISCV
import Clash.RISCV.Asm
import qualified Data.List

decode :: BitVector 32 -> Instr
decode instr =
  case opCode of
    0b0010011 ->
      addi (bv2i $ slice d11 d7 instr)
           (bv2i $ slice d19 d15 instr)
           (slice d31 d20 instr)
    0b1101111 ->
      jal (bv2i $ slice d11 d7 instr)
          (b20 ++# b19_12 ++# b11 ++# b10_1)
    _ ->
      errorX $ Data.List.intercalate " " [ "Could not decode instruction:", show instr, " opcode:",  show opCode ]
  where
    bit n = slice n n
    opCode = slice d6 d0 instr
    b20 = bit d31 instr
    b10_1 = slice d30 d21 instr
    b11 = bit d20 instr
    b19_12 = slice d19 d12 instr

encode :: Instr -> BitVector 32
encode (Imm op rd rs imm) =
    imm ++# pack rs ++# pack op ++# pack rd ++# (0b0010011 :: BitVector 7)
