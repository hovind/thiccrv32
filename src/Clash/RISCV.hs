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
fm pred succ rs1 000 rd 0001111 FENCE
000000000000 00000 000 00000 1110011 ECALL
000000000001 00000 000 00000 1110011 EBREAK
-}

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
