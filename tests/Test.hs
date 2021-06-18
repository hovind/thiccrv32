import Prelude
import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesFileExist, getCurrentDirectory, listDirectory)
import Test.Tasty

import qualified Tests.Clash.RISCV.Decode
import qualified Tests.Clash.RISCV.Spec

infixr 5  </>
(</>) :: FilePath -> FilePath -> FilePath
a </> b = a ++ "/" ++ b

getTests :: FilePath -> IO [FilePath]
getTests dir = do
  files <- fmap prefix . filter rv32ui <$> listDirectory dir
  filterM doesFileExist files
    where
      rv32ui base = "rv32ui-p" `isPrefixOf` base && not (".dump" `isSuffixOf` base)
      prefix file = dir </> file

main :: IO ()
main = do
  dir <- getCurrentDirectory
  tree <- getTests $ dir </> "riscv-tests" </> "isa"
  defaultMain $ testGroup "."
    [ Tests.Clash.RISCV.Decode.tests
    , Tests.Clash.RISCV.Spec.tests tree
    ]
