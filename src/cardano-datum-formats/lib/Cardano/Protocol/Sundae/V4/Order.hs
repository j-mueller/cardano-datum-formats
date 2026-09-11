{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Sundae.V4.Order (
    MultisigScript (..),
    Credential (..),
    StakingCredential (..),
    SundaeAddress (..),
    Destination (..),
    AssetAmount (..),
    BasicConstraint (..),
    SwapConstraint (..),
    FairnessConstraint (..),
    FeeConstraint (..),
    OrderConfig (..),
    OrderConstraint (..),
    mkOrderConstraint,
    swapIntentConstraints,
    OrderDatum (..),
) where

import Cardano.Api qualified as C
import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, sumOptions)
import Cardano.Protocol.Sundae.V3.Order (Credential (..), MultisigScript (..), StakingCredential (..), SundaeAddress (..))
import Cardano.Protocol.Sundae.V4.Pool (AssetAmount (..))
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

data BasicConstraint
    = BasicDeposit [AssetAmount] [AssetAmount]
    | BasicWithdraw [AssetAmount] [AssetAmount]
    | BasicSwap [AssetAmount] [AssetAmount]
    | BasicClaim [AssetAmount] [AssetAmount]
    deriving stock (Eq, Show, Generic)

instance Arbitrary BasicConstraint where
    arbitrary =
        Gen.oneof
            [ BasicDeposit <$> arbitrary <*> arbitrary
            , BasicWithdraw <$> arbitrary <*> arbitrary
            , BasicSwap <$> arbitrary <*> arbitrary
            , BasicClaim <$> arbitrary <*> arbitrary
            ]

instance PlutusTx.ToData BasicConstraint where
    toBuiltinData = \case
        BasicDeposit offered minReceived -> encodeBasicConstraint 0 offered minReceived
        BasicWithdraw offered minReceived -> encodeBasicConstraint 1 offered minReceived
        BasicSwap offered minReceived -> encodeBasicConstraint 2 offered minReceived
        BasicClaim offered minReceived -> encodeBasicConstraint 3 offered minReceived

instance PlutusTx.FromData BasicConstraint where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [offered, minReceived]) -> BasicDeposit <$> PlutusTx.fromBuiltinData offered <*> PlutusTx.fromBuiltinData minReceived
        (1, [offered, minReceived]) -> BasicWithdraw <$> PlutusTx.fromBuiltinData offered <*> PlutusTx.fromBuiltinData minReceived
        (2, [offered, minReceived]) -> BasicSwap <$> PlutusTx.fromBuiltinData offered <*> PlutusTx.fromBuiltinData minReceived
        (3, [offered, minReceived]) -> BasicClaim <$> PlutusTx.fromBuiltinData offered <*> PlutusTx.fromBuiltinData minReceived
        _ -> Nothing

data SwapConstraint = SwapConstraint
    { scOffered :: Asset
    , scOriginalOffered :: C.Quantity
    , scRemainingOffered :: C.Quantity
    , scMinReceived :: [AssetAmount]
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary SwapConstraint where
    arbitrary =
        SwapConstraint
            <$> arbitrary
            <*> (C.Quantity <$> arbitrary)
            <*> (C.Quantity <$> arbitrary)
            <*> arbitrary

instance PlutusTx.ToData SwapConstraint where
    toBuiltinData SwapConstraint{scOffered, scOriginalOffered = C.Quantity originalOffered, scRemainingOffered = C.Quantity remainingOffered, scMinReceived} =
        PlutusTx.mkConstr
            2
            [ PlutusTx.toBuiltinData scOffered
            , PlutusTx.mkI originalOffered
            , PlutusTx.mkI remainingOffered
            , PlutusTx.toBuiltinData scMinReceived
            ]

instance PlutusTx.FromData SwapConstraint where
    fromBuiltinData dt = D.withConstr dt $ \case
        (2, [offered, D.getI -> Just originalOffered, D.getI -> Just remainingOffered, minReceived]) ->
            SwapConstraint
                <$> PlutusTx.fromBuiltinData offered
                <*> pure (C.Quantity originalOffered)
                <*> pure (C.Quantity remainingOffered)
                <*> PlutusTx.fromBuiltinData minReceived
        _ -> Nothing

data FairnessConstraint = FairnessConstraint
    deriving stock (Eq, Show, Generic)

instance Arbitrary FairnessConstraint where
    arbitrary = pure FairnessConstraint

instance PlutusTx.ToData FairnessConstraint where
    toBuiltinData FairnessConstraint = PlutusTx.mkConstr 0 []

instance PlutusTx.FromData FairnessConstraint where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just FairnessConstraint
        _ -> Nothing

data FeeConstraint = FeeConstraint
    deriving stock (Eq, Show, Generic)

instance Arbitrary FeeConstraint where
    arbitrary = pure FeeConstraint

instance PlutusTx.ToData FeeConstraint where
    toBuiltinData FeeConstraint = PlutusTx.mkConstr 0 []

instance PlutusTx.FromData FeeConstraint where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just FeeConstraint
        _ -> Nothing

data OrderConfig = OrderConfig
    { orderConfigLabel :: PlutusTx.BuiltinByteString
    , orderConfigRequiredConstraints :: [PlutusTx.BuiltinByteString]
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary OrderConfig where
    arbitrary = OrderConfig <$> D.arbitraryBuiltinByteString <*> Gen.listOf D.arbitraryBuiltinByteString

instance PlutusTx.ToData OrderConfig where
    toBuiltinData OrderConfig{orderConfigLabel, orderConfigRequiredConstraints} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkB orderConfigLabel
            , PlutusTx.toBuiltinData orderConfigRequiredConstraints
            ]

instance PlutusTx.FromData OrderConfig where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just label, requiredConstraints]) ->
            OrderConfig label <$> PlutusTx.fromBuiltinData requiredConstraints
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

mkOrderConstraint :: (PlutusTx.ToData a) => PlutusTx.BuiltinByteString -> a -> OrderConstraint
mkOrderConstraint moduleHash = OrderConstraint moduleHash . PlutusTx.toBuiltinData

-- | Build the audit-final basic-swap and fee constraints used by a v4 swap intent.
swapIntentConstraints :: PlutusTx.BuiltinByteString -> PlutusTx.BuiltinByteString -> AssetAmount -> [AssetAmount] -> [OrderConstraint]
swapIntentConstraints basicModuleHash feeModuleHash offered minReceived =
    [ mkOrderConstraint basicModuleHash (BasicSwap [offered] minReceived)
    , mkOrderConstraint feeModuleHash FeeConstraint
    ]

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

encodeBasicConstraint :: Integer -> [AssetAmount] -> [AssetAmount] -> PlutusTx.BuiltinData
encodeBasicConstraint tag offered minReceived =
    PlutusTx.mkConstr tag [PlutusTx.toBuiltinData offered, PlutusTx.toBuiltinData minReceived]

instance ToJSON Destination where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON Destination where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''Destination)

instance Schema.ToSchema Destination where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON BasicConstraint where
    toJSON = Aeson.genericToJSON (sumOptions 5)
    toEncoding = Aeson.genericToEncoding (sumOptions 5)

instance FromJSON BasicConstraint where
    parseJSON = Aeson.genericParseJSON (sumOptions 5)

$(deriveTypeScript (sumOptions 5) ''BasicConstraint)

instance Schema.ToSchema BasicConstraint where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 5))

instance ToJSON SwapConstraint where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON SwapConstraint where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''SwapConstraint)

instance Schema.ToSchema SwapConstraint where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))

instance ToJSON FairnessConstraint where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON FairnessConstraint where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''FairnessConstraint)

instance Schema.ToSchema FairnessConstraint where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON FeeConstraint where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON FeeConstraint where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''FeeConstraint)

instance Schema.ToSchema FeeConstraint where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON OrderConfig where
    toJSON = Aeson.genericToJSON (jsonOptions 11)
    toEncoding = Aeson.genericToEncoding (jsonOptions 11)

instance FromJSON OrderConfig where
    parseJSON = Aeson.genericParseJSON (jsonOptions 11)

$(deriveTypeScript (jsonOptions 11) ''OrderConfig)

instance Schema.ToSchema OrderConfig where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 11))

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
