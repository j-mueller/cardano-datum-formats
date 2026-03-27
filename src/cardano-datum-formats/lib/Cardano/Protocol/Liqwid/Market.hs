{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Liqwid.Market (
    MarketDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Liqwid.Common (IntegerPair, fromList)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
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
    deriving stock (Eq, Show, Generic)

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

instance ToJSON MarketDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON MarketDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''MarketDatum)

instance Schema.ToSchema MarketDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
