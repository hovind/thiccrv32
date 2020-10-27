{-# LANGUAGE DataKinds, DuplicateRecordFields, NoImplicitPrelude #-}

module Clash.RISCV.Asm where

import Clash.Prelude
import Clash.RISCV

addi :: Register -> Register -> BitVector 12 -> Instr
addi = Imm AddI
