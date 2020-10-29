module Core where

import Clash.Prelude
import Clash.RISCV

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
continue cpu' = (cpu', CpuOut { peek = pc cpu', poke = Nothing })

compute :: Cpu -> Computation -> Register -> Value -> Value -> (Cpu, CpuOut)
compute cpu' comp rd rs2 rs1 =
    let value = case comp of
                    ALU op' -> alu op' rs2 rs1
                    BLU sign -> boolToBV $ cmp (BLT sign) rs2 rs1
    in continue $ next cpu' { registers = replace rd value $ registers cpu' }

load :: Cpu -> Width -> Register -> Value -> (Cpu, CpuOut)
load cpu' _ reg' value =
    -- TODO: Support different widths
    continue cpu' { stage = Executing, registers = replace reg' value $ registers cpu' }

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
        Imm op rd rs imm ->
            compute cpu' (computationI op) rd (reg cpu' rs) (signExtend imm)
        Load width rd rs imm ->
            (cpu' { stage = Loading width rd }, CpuOut { peek = addr $ alu AddA (reg cpu' rs) (signExtend imm), poke = Nothing })
        -- TODO: Support different widths
        Store _ rs2 rs1 imm ->
            let (cpu'', output) = continue $ next cpu'
            in (cpu'', output { poke = Just (addr $ alu AddA (reg cpu' rs2) (signExtend $ imm `shiftL` 1), reg cpu' rs1) })
        Branch branch rs2 rs1 imm ->
            let pc' = if cmp branch (reg cpu' rs2) (reg cpu' rs1)  then
                          addr $ alu AddA (val $ pc cpu') (signExtend $ imm `shiftL` 1)
                      else
                          incr $ pc cpu'
            in continue cpu' { pc = pc'}
        Jump (JAL rd imm) ->
            jump cpu' rd (val $ pc cpu') (signExtend $ imm `shiftL` 1)
        Jump (JALR rd rs imm) ->
            jump cpu' rd (reg cpu' rs) (signExtend imm)

incr :: Addr -> Addr
incr pc' = pc' + 4

next :: Cpu -> Cpu
next cpu' = cpu' { pc = incr $ pc cpu' }

decode :: BitVector 32 -> Instr
decode instr = undefined

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

data CpuOut' a b = CpuOut
    { peek :: !a
    , poke :: !b
    }

type CpuOut = CpuOut' Addr (Maybe (Addr, BitVector 32))

instance Bundle (CpuOut' a b) where
  type Unbundled t (CpuOut' a b) = CpuOut' (Signal t a) (Signal t b)
  bundle (CpuOut a b) = CpuOut <$> a <*> b
  unbundle out = CpuOut (peek <$> out) (poke <$> out)

data Cpu = Cpu
    { pc :: !Addr
    , stage :: !Stage
    , registers :: !(Vec NReg Value)
    , trap :: !()
    } deriving (Generic, NFDataX)

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
    deriving (Generic, NFDataX)

memory
  :: (HiddenClock dom, HiddenEnable dom)
  => Signal dom CpuOut
  -> Signal dom CpuIn
memory cpuOut = CpuIn <$> blockRamPow2 (repeat 0 :: Vec (2 ^ 32) (BitVector 32)) peek' poke'
  where
    CpuOut peek' poke' = unbundle cpuOut

system :: HiddenClockResetEnable dom => Signal dom (BitVector 32)
system = dataIn <$> memoryOut
  where
    memoryOut = memory cpuOut
    cpuOut = mealy cpu cpu' memoryOut
    cpu' = Cpu { pc = 0, stage = Initialising, registers = replicate d32 0, trap = () }

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
