module Main (main) where

import Cardano.Datum.Formats (supportedFormat)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

main :: IO ()
main =
  defaultMain $
    testGroup "cardano-datum-formats"
      [ testCase "exports the package identifier" $
          supportedFormat @?= "cardano-datum-formats"
      ]
