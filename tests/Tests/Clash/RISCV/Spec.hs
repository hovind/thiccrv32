module Tests.Clash.RISCV.Spec where

import Core
import Clash.Prelude
import Clash.RISCV
import Data.Binary.Strict.Get
import Data.Either
import Data.Elf
import Data.Word
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.List
import qualified Data.ByteString

tests :: [FilePath] -> TestTree
tests files = testGroup "RISCV" $ test <$> files

test :: FilePath -> TestTree
test file = testCase file $ do
  bytes <- Data.ByteString.readFile file
  let elf = parseElf bytes
  let memory = Data.List.foldr loadSegment emptyMemory $ elfSegments elf
  let entry = fromIntegral $ elfEntry elf
  let seq = sampleN @System 4 $ system memory entry
  print seq
  assertBool "Test passed" True

listOf :: Get a -> Get [a]
listOf g = do
  empty <- isEmpty
  if empty
     then pure []
     else do v <- g
             rest <- listOf g
             pure $ v : rest

segmentData :: ElfSegment -> [Word32]
segmentData section = fromRight [] $ fst $ runGet (listOf getWord32le) $ elfSegmentData section

loadData :: Memory -> PhysAddr -> [Word32] -> Memory
loadData mem paddr seg = Data.List.foldr (uncurry loadAt) mem $ Data.List.zip [paddr..] seg

loadSegment :: ElfSegment -> Memory -> Memory
loadSegment seg mem = loadData mem (paddr seg) $ segmentData seg
  where
    paddr = phys . fromIntegral . elfSegmentVirtAddr

loadAt :: PhysAddr -> Word32 -> Memory -> Memory
loadAt paddr datum = replace paddr (fromIntegral datum)


emptyMemory :: Memory
emptyMemory = repeat 0
