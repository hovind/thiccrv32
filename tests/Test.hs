import Prelude
import Control.Monad
import System.Directory
import Test.Tasty

import qualified Tests.Clash.RISCV.Decode
import qualified Tests.Clash.RISCV.Spec

getTests :: IO [FilePath]
getTests = do
  dir <- getCurrentDirectory
  files <- filter rv32ui <$> listDirectory dir
  filterM doesFileExist files
    where
      rv32ui = const True

main :: IO ()
main = do
  tree <- getTests
  defaultMain $ testGroup "."
    [ Tests.Clash.RISCV.Decode.tests
    , Tests.Clash.RISCV.Spec.tests tree
    ]
