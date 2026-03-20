module Main (main) where

import Cardano.Protocol.GeniusYield.Test qualified as GeniusYield
import Cardano.Protocol.Indigo.Test qualified as Indigo
import Cardano.Protocol.Liqwid.Test qualified as Liqwid
import Cardano.Protocol.MinSwap.Test qualified as MinSwap
import Cardano.Protocol.MuesliSwap.Test qualified as MuesliSwap
import Cardano.Protocol.Pulse.Test qualified as Pulse
import Cardano.Protocol.Strike.Test qualified as Strike
import Cardano.Protocol.Sundae.Test qualified as Sundae
import Cardano.Protocol.VyFinance.Test qualified as VyFinance
import Cardano.Protocol.WingRiders.Test qualified as WingRiders
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "cardano-datum-formats"
            [ GeniusYield.tests
            , Indigo.tests
            , Liqwid.tests
            , MinSwap.tests
            , MuesliSwap.tests
            , Pulse.tests
            , Strike.tests
            , Sundae.tests
            , VyFinance.tests
            , WingRiders.tests
            ]
