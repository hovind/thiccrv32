{-# LANGUAGE DataKinds, DuplicateRecordFields, NamedFieldPuns, NoImplicitPrelude #-}
module Core where

import Clash.Prelude

-- ADDI, SLTI[U], ANDI, ORI, XORI, SLLI; SRLI, SRAI,LUI, AUIPC, ADD, SLT,SLTU,AND,OR,XOR,SLL,SRL,SUB,SRA,NOP,JAL, JALR,BEQ,BNE,BLT[U],BGE[U],LOAD,STORE,ECALL,EBREAK
data Funct3 = Funct3

data Funct7 = Funct7

data Instr
    = Reg !Op !Register !Register !Register
    -- Register?
    | Imm ImmInstr
    -- Immediate?
    | Load !Width !Register !Register !(BitVector 12)
    -- LUI (load upper immediate) is used to build 32-bit constants and uses the U-type format
    | Store StoreInstr
    -- Loads are encoded in the I-type format and stores are S-type
    | Branch BranchInstr
    -- All branch instructions use the B-type instruction format
    | Jump JumpInstr
    -- The jump and link (JAL) instruction uses the J-type format

data Op
    = Add
    | Sub
    | SLT Sign
    | And
    | Or
    | XOr
    | SLL
    | SRL
    | SRA

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
    | BLU Sign

computationI :: OpI -> Computation
computationI op =
    case op of
        AddI -> ALU AddA
        SLTI sign -> BLU sign
        AndI -> ALU AndA
        OrI -> ALU OrA
        XOrI -> ALU XOrA
        SLLI -> ALU SLLA
        SRLI -> ALU SRLA
        SRAI -> ALU SRAA

computation :: Op -> Computation
computation op =
    case op of
        Add -> ALU AddA
        Sub -> ALU SubA
        SLT sign -> BLU sign
        And -> ALU AndA
        Or -> ALU OrA
        XOr -> ALU XOrA
        SLL -> ALU SLLA
        SRL -> ALU SRLA
        SRA -> ALU SRAA

-- Arithmetic operation
data OpA
    = AddA
    | SubA
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

data StoreInstr = StoreInstr
    { simm :: !(BitVector 12)
    , srs2 :: !Register
    , srs1 :: !Register
    , width :: !Width
    }

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

data JumpInstr
    = JAL !Register !(BitVector 20)
    | JALR !Register !Register !(BitVector 12)

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
continue cpu' = (cpu', CpuOut { read = pc cpu', write = Nothing })


load :: Cpu -> Width -> Register -> Value -> (Cpu, CpuOut)
load cpu' width' reg' value = continue cpu' { stage = Executing, registers = replace reg' value $ registers cpu' }

compute :: Cpu -> Computation -> Register -> Value -> Value -> (Cpu, CpuOut)
compute cpu' comp rd rs2 rs1 =
    let value = case comp of
                    ALU op' -> alu op' rs2 rs1
                    BLU sign -> boolToBV $ cmp (BLT sign) rs2 rs1
    in continue $ next cpu' { registers = replace rd value $ registers cpu' }

jump :: Cpu -> Register -> Value -> Value -> (Cpu, CpuOut)
jump cpu' rd base offset =
    continue cpu' { pc = addr $ alu AddA base offset
                  , registers = replace rd (val $ incr $ pc cpu') $ registers cpu'
                  }


exec :: Cpu -> Instr -> (Cpu, CpuOut)
exec cpu' instr =
    case instr of
        Reg op rd rs2 rs1 ->
            compute cpu' (computation op) rd (reg cpu' rs2) (reg cpu' rs1)
        Imm (ImmInstr { iop, ird, irs, iimm }) ->
            compute cpu' (computationI iop) ird (reg cpu' irs) iimm
        Load width rd rs imm ->
            (cpu' { stage = Loading width rd }, CpuOut { read = addr $ alu AddA (reg cpu' rs) (extend imm), write = Nothing })
        Store (StoreInstr {}) ->
            undefined
        Branch (BranchInstr { branch, brs2, brs1, bimm}) ->
            let pc' = if cmp branch (reg cpu' brs2) (reg cpu' brs1)  then
                          addr $ alu AddA (val $ pc cpu') bimm
                      else
                          incr $ pc cpu'
            in continue cpu' { pc = pc'}
        Jump (JAL rd imm) ->
            jump cpu' rd (val $ pc cpu') (pack $ signExtend $ imm `shiftL` 1)
        Jump (JALR rd rs imm) ->
            jump cpu' rd (reg cpu' rd) (pack $ signExtend imm)

incr :: Addr -> Addr
incr pc' = pc' + 4

next :: Cpu -> Cpu
next cpu' = cpu' { pc = incr $ pc cpu' }

decode :: BitVector 32 -> Instr
decode instr = undefined


type XLen = 32 -- Register width
type NReg = 32 -- Number of registers

type Word = BitVector 32
type Value = BitVector XLen
type Register = Index NReg

type Addr = Unsigned 32

val :: Addr -> Value
val = pack

addr :: Value -> Addr
addr = unpack

signed :: KnownNat n => BitVector n -> Signed n
signed = unpack

unsigned :: KnownNat n => BitVector n -> Unsigned n
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
    | Loading !Width !Register -- Don't advance PC

cpu :: Cpu -> CpuIn -> (Cpu, CpuOut)
cpu cpu' input =
    case stage cpu' of
        Initialising ->
            continue $ cpu' { stage = Executing }
        Executing ->
            exec cpu' $ decode $ dataIn input
        Loading width' reg' ->
            load cpu' width' reg' $ dataIn input



alu :: OpA -> Value -> Value -> Value
alu op rs2 rs1 =
    case op of
        AddA -> rs2 + rs1
        SubA -> rs2 - rs1
        AndA -> rs2 .&. rs1
        OrA -> rs2 .|. rs1
        XOrA -> rs2 `xor` rs1
        SLLA -> rs2 `shiftL` unpack (resize $ slice d4 d0 rs1) -- TODO: This resize shift is BS
        SRLA -> rs2 `shiftR` unpack (resize $ slice d4 d0 rs1) -- TODO: This resize is also BS
        SRAA -> pack $ signed rs2 `shiftR` unpack (resize $ slice d4 d0 rs1)



-- bootloader :: File ->
