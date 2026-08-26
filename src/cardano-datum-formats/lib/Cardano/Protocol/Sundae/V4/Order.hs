{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Sundae.V4.Order (
    MultisigScript (..),
    Credential (..),
    StakingCredential (..),
    SundaeAddress (..),
    Destination (..),
    OrderConstraint (..),
    OrderDatum (..),
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, sumOptions)
import Cardano.Protocol.Sundae.V3.Order (Credential (..), MultisigScript (..), StakingCredential (..), SundaeAddress (..))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data Destination
    = FixedDestination SundaeAddress (Maybe PlutusTx.BuiltinData)
    | SelfDestination
    deriving stock (Eq, Show, Generic)

instance Arbitrary Destination where
    arbitrary =
        Gen.oneof
            [ FixedDestination <$> arbitrary <*> arbitraryOptionalData
            , pure SelfDestination
            ]

instance PlutusTx.ToData Destination where
    toBuiltinData = \case
        FixedDestination address datum ->
            PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData address, D.toOption id datum]
        SelfDestination -> PlutusTx.mkConstr 1 []

instance PlutusTx.FromData Destination where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [address, datum]) ->
            FixedDestination
                <$> PlutusTx.fromBuiltinData address
                <*> D.fromOption Just datum
        (1, []) -> Just SelfDestination
        _ -> Nothing

data OrderConstraint = OrderConstraint
    { ocModuleHash :: PlutusTx.BuiltinByteString
    , ocData :: PlutusTx.BuiltinData
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary OrderConstraint where
    arbitrary = OrderConstraint <$> D.arbitraryBuiltinByteString <*> D.arbitraryBuiltinData

instance PlutusTx.ToData OrderConstraint where
    toBuiltinData OrderConstraint{ocModuleHash, ocData} =
        PlutusTx.mkList [PlutusTx.mkB ocModuleHash, ocData]

instance PlutusTx.FromData OrderConstraint where
    fromBuiltinData dt = D.fromList dt $ \case
        [D.getB -> Just ocModuleHash, ocData] -> Just OrderConstraint{ocModuleHash, ocData}
        _ -> Nothing

data OrderDatum = OrderDatum
    { odOwner :: MultisigScript
    , odDestination :: Destination
    , odServiceBudget :: C.Quantity
    , odMaxPerExecution :: C.Quantity
    , odConfigToken :: PlutusTx.BuiltinByteString
    , odConstraints :: [OrderConstraint]
    , odExtension :: PlutusTx.BuiltinData
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary OrderDatum where
    arbitrary =
        OrderDatum
            <$> arbitrary
            <*> arbitrary
            <*> (C.Quantity <$> arbitrary)
            <*> (C.Quantity <$> arbitrary)
            <*> D.arbitraryBuiltinByteString
            <*> arbitrary
            <*> D.arbitraryBuiltinData

instance PlutusTx.ToData OrderDatum where
    toBuiltinData OrderDatum{odOwner, odDestination, odServiceBudget = C.Quantity serviceBudget, odMaxPerExecution = C.Quantity maxPerExecution, odConfigToken, odConstraints, odExtension} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData odOwner
            , PlutusTx.toBuiltinData odDestination
            , PlutusTx.mkI serviceBudget
            , PlutusTx.mkI maxPerExecution
            , PlutusTx.mkB odConfigToken
            , PlutusTx.toBuiltinData odConstraints
            , odExtension
            ]

instance PlutusTx.FromData OrderDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [owner, destination, serviceBudget, maxPerExecution, D.getB -> Just configToken, constraints, extension]) ->
            OrderDatum
                <$> PlutusTx.fromBuiltinData owner
                <*> PlutusTx.fromBuiltinData destination
                <*> fmap C.Quantity (D.getI serviceBudget)
                <*> fmap C.Quantity (D.getI maxPerExecution)
                <*> pure configToken
                <*> PlutusTx.fromBuiltinData constraints
                <*> pure extension
        _ -> Nothing

arbitraryOptionalData :: Gen.Gen (Maybe PlutusTx.BuiltinData)
arbitraryOptionalData = Gen.oneof [pure Nothing, Just <$> D.arbitraryBuiltinData]

instance ToJSON Destination where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON Destination where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''Destination)

instance Schema.ToSchema Destination where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON OrderConstraint where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON OrderConstraint where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''OrderConstraint)

instance Schema.ToSchema OrderConstraint where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON OrderDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON OrderDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''OrderDatum)

instance Schema.ToSchema OrderDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
