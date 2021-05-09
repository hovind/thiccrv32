import Prelude

import Test.Tasty

import qualified Tests.Clash.RISCV.Decode

main :: IO ()
main = defaultMain $ testGroup "."
  [ Tests.Clash.RISCV.Decode.tests
  ]
