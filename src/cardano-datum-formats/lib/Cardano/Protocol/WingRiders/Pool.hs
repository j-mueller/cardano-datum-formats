{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.WingRiders.Pool (
    FeeFrom (..),
    WingRidersPoolDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions, sumOptions)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.ByteString qualified as BS
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Word (Word8)
import GHC.Generics (Generic)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data FeeFrom
    = InputToken
    | OutputToken
    | TokenA
    | TokenB
    deriving stock (Eq, Show, Generic)

instance Arbitrary FeeFrom where
    arbitrary = do
        n <- arbitrary @Int
        pure $ case n `mod` 4 of
            0 -> InputToken
            1 -> OutputToken
            2 -> TokenA
            _ -> TokenB

instance PTx.ToData FeeFrom where
    toBuiltinData = \case
        InputToken -> PlutusTx.mkConstr 0 []
        OutputToken -> PlutusTx.mkConstr 1 []
        TokenA -> PlutusTx.mkConstr 2 []
        TokenB -> PlutusTx.mkConstr 3 []

instance PTx.FromData FeeFrom where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just InputToken
        (1, []) -> Just OutputToken
        (2, []) -> Just TokenA
        (3, []) -> Just TokenB
        _ -> Nothing

data WingRidersPoolDatum = WingRidersPoolDatum
    { wrpdAssetAPolicyId :: PlutusTx.BuiltinByteString
    , wrpdAssetAName :: PlutusTx.BuiltinByteString
    , wrpdAssetBPolicyId :: PlutusTx.BuiltinByteString
    , wrpdAssetBName :: PlutusTx.BuiltinByteString
    , wrpdTreasuryA :: Integer
    , wrpdTreasuryB :: Integer
    , wrpdFeeFrom :: FeeFrom
    , wrpdTreasuryAuthorityPolicyId :: PlutusTx.BuiltinByteString
    , wrpdTreasuryAuthorityAssetName :: PlutusTx.BuiltinByteString
    , wrpdTreasuryFeePointsAToB :: Integer
    , wrpdTreasuryFeePointsBToA :: Integer
    , wrpdSwapFeePointsAToB :: Integer
    , wrpdSwapFeePointsBToA :: Integer
    , wrpdFeeBasis :: Integer
    , wrpdSharesAssetName :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary WingRidersPoolDatum where
    arbitrary =
        WingRidersPoolDatum
            <$> arbitraryPolicyId
            <*> arbitraryAssetName
            <*> arbitraryPolicyId
            <*> arbitraryAssetName
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryPolicyId
            <*> arbitraryAssetName
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryBuiltinByteString

arbitraryPolicyId :: QC.Gen PlutusTx.BuiltinByteString
arbitraryPolicyId = arbitraryBuiltinByteString

arbitraryAssetName :: QC.Gen PlutusTx.BuiltinByteString
arbitraryAssetName = arbitraryBuiltinByteString

arbitraryBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString = PlutusTx.toBuiltin . BS.pack <$> (arbitrary :: QC.Gen [Word8])

instance PTx.ToData WingRidersPoolDatum where
    toBuiltinData
        WingRidersPoolDatum
            { wrpdAssetAPolicyId
            , wrpdAssetAName
            , wrpdAssetBPolicyId
            , wrpdAssetBName
            , wrpdTreasuryA
            , wrpdTreasuryB
            , wrpdFeeFrom
            , wrpdTreasuryAuthorityPolicyId
            , wrpdTreasuryAuthorityAssetName
            , wrpdTreasuryFeePointsAToB
            , wrpdTreasuryFeePointsBToA
            , wrpdSwapFeePointsAToB
            , wrpdSwapFeePointsBToA
            , wrpdFeeBasis
            , wrpdSharesAssetName
            } =
            PlutusTx.mkConstr
                0
                [ PlutusTx.mkB wrpdAssetAPolicyId
                , PlutusTx.mkB wrpdAssetAName
                , PlutusTx.mkB wrpdAssetBPolicyId
                , PlutusTx.mkB wrpdAssetBName
                , PlutusTx.mkI wrpdTreasuryA
                , PlutusTx.mkI wrpdTreasuryB
                , PTx.toBuiltinData wrpdFeeFrom
                , PlutusTx.mkB wrpdTreasuryAuthorityPolicyId
                , PlutusTx.mkB wrpdTreasuryAuthorityAssetName
                , PlutusTx.mkI wrpdTreasuryFeePointsAToB
                , PlutusTx.mkI wrpdTreasuryFeePointsBToA
                , PlutusTx.mkI wrpdSwapFeePointsAToB
                , PlutusTx.mkI wrpdSwapFeePointsBToA
                , PlutusTx.mkI wrpdFeeBasis
                , PlutusTx.mkB wrpdSharesAssetName
                ]

instance PTx.FromData WingRidersPoolDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [ D.getB -> Just wrpdAssetAPolicyId
            , D.getB -> Just wrpdAssetAName
            , D.getB -> Just wrpdAssetBPolicyId
            , D.getB -> Just wrpdAssetBName
            , D.getI -> Just wrpdTreasuryA
            , D.getI -> Just wrpdTreasuryB
            , feeFrom
            , D.getB -> Just wrpdTreasuryAuthorityPolicyId
            , D.getB -> Just wrpdTreasuryAuthorityAssetName
            , D.getI -> Just wrpdTreasuryFeePointsAToB
            , D.getI -> Just wrpdTreasuryFeePointsBToA
            , D.getI -> Just wrpdSwapFeePointsAToB
            , D.getI -> Just wrpdSwapFeePointsBToA
            , D.getI -> Just wrpdFeeBasis
            , D.getB -> Just wrpdSharesAssetName
            ]) -> do
                wrpdFeeFrom <- PTx.fromBuiltinData feeFrom
                pure
                    WingRidersPoolDatum
                        { wrpdAssetAPolicyId
                        , wrpdAssetAName
                        , wrpdAssetBPolicyId
                        , wrpdAssetBName
                        , wrpdTreasuryA
                        , wrpdTreasuryB
                        , wrpdFeeFrom
                        , wrpdTreasuryAuthorityPolicyId
                        , wrpdTreasuryAuthorityAssetName
                        , wrpdTreasuryFeePointsAToB
                        , wrpdTreasuryFeePointsBToA
                        , wrpdSwapFeePointsAToB
                        , wrpdSwapFeePointsBToA
                        , wrpdFeeBasis
                        , wrpdSharesAssetName
                        }
        _ -> Nothing

instance ToJSON FeeFrom where
    toJSON = Aeson.genericToJSON (sumOptions 0)
    toEncoding = Aeson.genericToEncoding (sumOptions 0)

instance FromJSON FeeFrom where
    parseJSON = Aeson.genericParseJSON (sumOptions 0)

$(deriveTypeScript (sumOptions 0) ''FeeFrom)

instance Schema.ToSchema FeeFrom where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (sumOptions 0))

instance ToJSON WingRidersPoolDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 4)
    toEncoding = Aeson.genericToEncoding (jsonOptions 4)

instance FromJSON WingRidersPoolDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 4)

$(deriveTypeScript (jsonOptions 4) ''WingRidersPoolDatum)

instance Schema.ToSchema WingRidersPoolDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 4))
