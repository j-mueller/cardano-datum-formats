{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Liqwid.Loan (
    LoanDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Cardano.Protocol.Liqwid.Common (PubKeyHash, fromList)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data LoanDatum = LoanDatum
    { ldBorrower :: PubKeyHash
    , ldPrincipal :: Integer
    , ldAccruedInterest :: Integer
    , ldMinInterest :: Integer
    , ldInterestIndex :: Integer
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary LoanDatum where
    arbitrary = LoanDatum <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance PTx.ToData LoanDatum where
    toBuiltinData LoanDatum{ldBorrower, ldPrincipal, ldAccruedInterest, ldMinInterest, ldInterestIndex} =
        PlutusTx.mkList
            [ PTx.toBuiltinData ldBorrower
            , PlutusTx.mkI ldPrincipal
            , PlutusTx.mkI ldAccruedInterest
            , PlutusTx.mkI ldMinInterest
            , PlutusTx.mkI ldInterestIndex
            ]

instance PTx.FromData LoanDatum where
    fromBuiltinData dt = do
        [borrower, D.getI -> Just ldPrincipal, D.getI -> Just ldAccruedInterest, D.getI -> Just ldMinInterest, D.getI -> Just ldInterestIndex] <- fromList dt
        ldBorrower <- PTx.fromBuiltinData borrower
        pure LoanDatum{ldBorrower, ldPrincipal, ldAccruedInterest, ldMinInterest, ldInterestIndex}

instance ToJSON LoanDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON LoanDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''LoanDatum)

instance Schema.ToSchema LoanDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
