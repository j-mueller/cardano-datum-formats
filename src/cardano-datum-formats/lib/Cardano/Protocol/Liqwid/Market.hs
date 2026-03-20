module Cardano.Protocol.Liqwid.Market (
    MarketDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Liqwid.Common (IntegerPair, fromList)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data MarketDatum = MarketDatum
    { mdSupply :: Integer
    , mdReserve :: Integer
    , mdQTokens :: Integer
    , mdPrincipal :: Integer
    , mdInterest :: Integer
    , mdInterestIndex :: Integer
    , mdInterestRate :: IntegerPair
    , mdLastInterestTime :: Integer
    , mdLastBatch :: Integer
    , mdQTokenRate :: IntegerPair
    , mdMinAda :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary MarketDatum where
    arbitrary =
        MarketDatum
            <$> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData MarketDatum where
    toBuiltinData MarketDatum{mdSupply, mdReserve, mdQTokens, mdPrincipal, mdInterest, mdInterestIndex, mdInterestRate, mdLastInterestTime, mdLastBatch, mdQTokenRate, mdMinAda} =
        PlutusTx.mkList
            [ PlutusTx.mkI mdSupply
            , PlutusTx.mkI mdReserve
            , PlutusTx.mkI mdQTokens
            , PlutusTx.mkI mdPrincipal
            , PlutusTx.mkI mdInterest
            , PlutusTx.mkI mdInterestIndex
            , PTx.toBuiltinData mdInterestRate
            , PlutusTx.mkI mdLastInterestTime
            , PlutusTx.mkI mdLastBatch
            , PTx.toBuiltinData mdQTokenRate
            , PlutusTx.mkI mdMinAda
            ]

instance PTx.FromData MarketDatum where
    fromBuiltinData dt = do
        [ D.getI -> Just mdSupply
            , D.getI -> Just mdReserve
            , D.getI -> Just mdQTokens
            , D.getI -> Just mdPrincipal
            , D.getI -> Just mdInterest
            , D.getI -> Just mdInterestIndex
            , interestRate
            , D.getI -> Just mdLastInterestTime
            , D.getI -> Just mdLastBatch
            , qTokenRate
            , D.getI -> Just mdMinAda
            ] <- fromList dt
        mdInterestRate <- PTx.fromBuiltinData interestRate
        mdQTokenRate <- PTx.fromBuiltinData qTokenRate
        pure MarketDatum{mdSupply, mdReserve, mdQTokens, mdPrincipal, mdInterest, mdInterestIndex, mdInterestRate, mdLastInterestTime, mdLastBatch, mdQTokenRate, mdMinAda}
