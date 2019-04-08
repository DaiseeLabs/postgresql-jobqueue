module Main where

import qualified Spec
import           Test.Hspec.Core.Hooks
import           Test.Hspec.Runner

main :: IO ()
main = do
  rebuildTestDB
  hspecWith defaultConfig $ before_ (withTestConnection truncateTestDatabase) Spec.spec
