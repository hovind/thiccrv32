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

{-
RV32I Base Instruction Set
imm[31:12] rd 0110111 LUI
imm[31:12] rd 0010111 AUIPC
imm[20|10:1|11|19:12] rd 1101111 JAL
imm[11:0] rs1 000 rd 1100111 JALR
imm[12|10:5] rs2 rs1 000 imm[4:1|11] 1100011 BEQ
imm[12|10:5] rs2 rs1 001 imm[4:1|11] 1100011 BNE
imm[12|10:5] rs2 rs1 100 imm[4:1|11] 1100011 BLT
imm[12|10:5] rs2 rs1 101 imm[4:1|11] 1100011 BGE
imm[12|10:5] rs2 rs1 110 imm[4:1|11] 1100011 BLTU
imm[12|10:5] rs2 rs1 111 imm[4:1|11] 1100011 BGEU
imm[11:0] rs1 000 rd 0000011 LB
imm[11:0] rs1 001 rd 0000011 LH
imm[11:0] rs1 010 rd 0000011 LW
imm[11:0] rs1 100 rd 0000011 LBU
imm[11:0] rs1 101 rd 0000011 LHU
imm[11:5] rs2 rs1 000 imm[4:0] 0100011 SB
imm[11:5] rs2 rs1 001 imm[4:0] 0100011 SH
imm[11:5] rs2 rs1 010 imm[4:0] 0100011 SW
imm[11:0] rs1 000 rd 0010011 ADDI
imm[11:0] rs1 010 rd 0010011 SLTI
imm[11:0] rs1 011 rd 0010011 SLTIU
imm[11:0] rs1 100 rd 0010011 XORI
imm[11:0] rs1 110 rd 0010011 ORI
imm[11:0] rs1 111 rd 0010011 ANDI
0000000 shamt rs1 001 rd 0010011 SLLI
0000000 shamt rs1 101 rd 0010011 SRLI
0100000 shamt rs1 101 rd 0010011 SRAI
0000000 rs2 rs1 000 rd 0110011 ADD
0100000 rs2 rs1 000 rd 0110011 SUB
0000000 rs2 rs1 001 rd 0110011 SLL
0000000 rs2 rs1 010 rd 0110011 SLT
0000000 rs2 rs1 011 rd 0110011 SLTU
0000000 rs2 rs1 100 rd 0110011 XOR
0000000 rs2 rs1 101 rd 0110011 SRL
0100000 rs2 rs1 101 rd 0110011 SRA
0000000 rs2 rs1 110 rd 0110011 OR
0000000 rs2 rs1 111 rd 0110011 AND
0000 pred succ rs1 000 rd 0001111 FENCE
000000000000 00000 000 00000 1110011 ECALL
000000000001 00000 000 00000 1110011 EBREAK
-}
