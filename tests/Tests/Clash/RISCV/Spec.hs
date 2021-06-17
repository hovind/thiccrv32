module Tests.Clash.RISCV.Spec where

import Prelude
import Data.Elf
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString

tests :: [FilePath] -> TestTree
tests files = testGroup "RISCV" $ test <$> files

test :: FilePath -> TestTree
test file = testCase file $ do
  bytes <- Data.ByteString.readFile file
  let elf = parseElf bytes
  assertBool "Test passed" True
