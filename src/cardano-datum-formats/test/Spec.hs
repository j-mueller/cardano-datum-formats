module Main (main) where

import Cardano.Protocol.MinSwap.Test qualified as MinSwap
import Cardano.Protocol.Sundae.Test qualified as Sundae
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "cardano-datum-formats" [MinSwap.tests, Sundae.tests]
