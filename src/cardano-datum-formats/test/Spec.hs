module Main (main) where

import Cardano.Protocol.MinSwap.Test qualified as MinSwap
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain MinSwap.tests
