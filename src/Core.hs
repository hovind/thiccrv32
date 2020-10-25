{-# LANGUAGE DataKinds, DuplicateRecordFields, NamedFieldPuns, NoImplicitPrelude #-}
module Core where

import Clash.Prelude

-- ADDI, SLTI[U], ANDI, ORI, XORI, SLLI; SRLI, SRAI,LUI, AUIPC, ADD, SLT,SLTU,AND,OR,XOR,SLL,SRL,SUB,SRA,NOP,JAL, JALR,BEQ,BNE,BLT[U],BGE[U],LOAD,STORE,ECALL,EBREAK
data Funct3 = Funct3

data Funct7 = Funct7

data Instr
    = Reg !RegInstr
    -- Register?
    | Imm ImmInstr
    -- Immediate?
    | Load LoadInstr
    -- LUI (load upper immediate) is used to build 32-bit constants and uses the U-type format
    | Store StoreInstr
    -- Loads are encoded in the I-type format and stores are S-type
    | Branch BranchInstr
    -- All branch instructions use the B-type instruction format
    | Jump JumpInstr
    -- The jump and link (JAL) instruction uses the J-type format

data Op
    = Add
    | SLT Sign
    | And
    | Or
    | XOr
    | SLL
    | SRL
    | Sub
    | SRA

data RegInstr = RegInstr
    { rop :: !Op
    , rrs2 :: !Register
    , rrs1 :: !Register
    , rrd :: !Register
    }

data OpI
    = AddI
    | SLTI Sign
    | AndI
    | OrI
    | XOrI
    | SLLI
    | SRLI
    | SRAI

data Computation
    = ALU OpA
    | BLU Branch

computeI :: OpI -> Computation
computeI = undefined

compute :: Op -> Computation
compute = undefined

-- Arithmetic operation
data OpA
    = AddA
    | AndA
    | OrA
    | XOrA
    | SLLA
    | SRLA
    | SRAA

data ImmInstr = ImmInstr
    { iop :: !OpI
    , irs :: !Register
    , iimm :: !Value
    , ird :: !Register
    }

data UpperImmInstr = UpperImmInstr
    { uimm :: !Value
    , urs :: !Register
    , func3 :: !Funct3
    , urd :: !Register
    }

data LoadInstr = LoadInstr

data StoreInstr = StoreInstr
    { simm :: !(BitVector 12)
    , srs2 :: !Register
    , srs1 :: !Register
    , width :: !Width
    }

data Width
    = Word
    | HalfWord
    | Byte

data Sign
    = Signed
    | Unsigned

data Branch
    = BEq
    | BNE
    | BLT Sign
    | BGE Sign

{-data Op
    = Add
    -- Set less than
    | And
    | Or
    | Xor
    | Sr
    -- Shift right
    | Sl
    -- Shift left
-}

data BranchInstr = BranchInstr
    { bimm :: !Value
    , brs2 :: !Register
    , brs1 :: !Register
    , branch :: !Branch
    }

data JumpInstr = JumpInstr
    { jimm :: !(BitVector 20)
    , jrd :: !Register
    }

cmp :: Branch -> Value -> Value -> Bool
cmp branch lhs rhs =
    case branch of
        BEq ->
            lhs == rhs
        BNE ->
            lhs /= rhs
        BLT Unsigned->
            unsigned lhs < unsigned rhs
        BGE Unsigned ->
            unsigned lhs >= unsigned rhs
        BLT Signed ->
            signed lhs < signed rhs
        BGE Signed ->
            signed lhs >= signed rhs

reg :: Cpu -> Register -> Value
reg = (!!) . registers

continue :: Cpu -> (Cpu, CpuOut)
continue cpu' = undefined


load :: Cpu -> Register -> Value -> (Cpu, CpuOut)
load cpu' reg value = continue cpu' { stage = Executing, registers = replace reg value $ registers cpu' }


exec :: Cpu -> Instr -> (Cpu, CpuOut)
exec cpu' instr =
    case instr of
        Reg (RegInstr { rop, rrd, rrs2, rrs1 }) ->
            let value = alu (compute rop) (reg cpu' rrs2) (reg cpu' rrs1)
            in continue cpu' { registers = replace rrd value $ registers cpu' }
        Imm (ImmInstr { iop, ird, irs, iimm }) ->
            let value = alu (computeI iop) (reg cpu' irs) iimm
            in continue cpu' { registers = replace ird value $ registers cpu' }
        Branch (BranchInstr { branch, brs2, brs1, bimm}) ->
            let pc' = if cmp branch (reg cpu' brs2) (reg cpu' brs1)  then
                          address $ alu (ALU AddA) (value $ pc cpu') bimm
                      else
                          pc cpu' + 1
            in
                continue cpu'{ pc = pc'}

decode :: BitVector 32 -> Instr
decode instr = undefined


type XLen = 32 -- Register width
type NReg = 32 -- Number of registers

type Word = BitVector 32
type Value = BitVector XLen
type Register = Index NReg

type Addr = Unsigned 32

value :: Addr -> Value
value = pack

address :: Value -> Addr
address = unpack

signed :: Value -> Signed 32
signed = unpack

unsigned :: Value -> Unsigned 32
unsigned = unpack

newtype CpuIn = CpuIn
    { dataIn :: Value
    }

data CpuOut = CpuOut
    { read :: !Addr
    , write :: !(Maybe (Addr, Value))
    }

data Cpu = Cpu
    { pc :: !Addr
    , stage :: !Stage
    , registers :: !(Vec NReg Value)
    , trap :: !()
    }

-- BUSES
-- READ: LOAD, READ INSTR
-- WRITE: STORE



-- LOAD:
-- 1. Exec: Data addr
-- 2. Load reg: Instr addr
data Stage
    = Initialising -- Waiting for first instruction
    | Executing -- Advance PC
    | Loading Register -- Don't advance PC

cpu :: Cpu -> CpuIn -> (Cpu, CpuOut)
cpu cpu' input =
    case stage cpu' of
        Initialising ->
            continue $ cpu' { stage = Executing }
        Executing ->
            exec cpu' $ decode $ dataIn input
        Loading reg ->
            load cpu' reg $ dataIn input



alu :: Computation -> Value -> Value -> Value
alu = undefined



-- bootloader :: File ->
