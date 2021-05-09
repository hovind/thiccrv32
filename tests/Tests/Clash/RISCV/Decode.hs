module Tests.Clash.RISCV.Decode where

import Prelude

import Test.Tasty
import Test.Tasty.Hedgehog

import Hedgehog

example :: Property
example = property $ do
  True === True

tests :: TestTree
tests = testProperty "Example" example
