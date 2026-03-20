module Main (main) where

import Cardano.Protocol.Indigo.Test qualified as Indigo
import Cardano.Protocol.Liqwid.Test qualified as Liqwid
import Cardano.Protocol.MinSwap.Test qualified as MinSwap
import Cardano.Protocol.Pulse.Test qualified as Pulse
import Cardano.Protocol.Sundae.Test qualified as Sundae
import Cardano.Protocol.WingRiders.Test qualified as WingRiders
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "cardano-datum-formats" [Indigo.tests, Liqwid.tests, MinSwap.tests, Pulse.tests, Sundae.tests, WingRiders.tests]
