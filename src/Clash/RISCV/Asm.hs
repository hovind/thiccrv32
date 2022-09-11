module Clash.RISCV.Asm where

import Clash.Prelude
import Clash.RISCV


hi :: Addr -> BitVector 20
hi = slice d31 d12

lo :: Addr -> BitVector 12
lo = slice d11 d0

x0, x1, x2 :: Register
x0 = 0
x1 = 1
x2 = 2

addi :: Register -> Register -> BitVector 12 -> Instr
addi = Imm AddI

jal :: Register -> BitVector 20 -> Instr
jal rd = Jump . JAL rd

mov :: Register -> Register -> Instr
mov rd rs = addi rd rs 0

nop :: Instr
nop = addi x0 x0 0
