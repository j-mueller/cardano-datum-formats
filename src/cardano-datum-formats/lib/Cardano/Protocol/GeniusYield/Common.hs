module Cardano.Protocol.GeniusYield.Common (
    Address (..),
    AssetClass (..),
    Credential (..),
    OutputReference (..),
    RationalD (..),
    StakeCredential (..),
    Value (..),
    arbitraryAnyBuiltinByteString,
    arbitraryBuiltinByteString,
    fromMap,
    maybeFromOptionData,
    maybeToOptionData,
) where

import Cardano.Data qualified as D
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data OutputReference = OutputReference
    { orTransactionId :: PlutusTx.BuiltinByteString
    , orOutputIndex :: Integer
    }
    deriving stock (Eq, Ord, Show)

data Credential
    = PublicKeyCredential PlutusTx.BuiltinByteString
    | ScriptCredential PlutusTx.BuiltinByteString
    deriving stock (Eq, Ord, Show)

data StakeCredential
    = Inline Credential
    | Pointer
        { pointerSlotNumber :: Integer
        , pointerTransactionIndex :: Integer
        , pointerCertificateIndex :: Integer
        }
    deriving stock (Eq, Ord, Show)

data Address = Address
    { addressPaymentCredential :: Credential
    , addressStakeCredential :: Maybe StakeCredential
    }
    deriving stock (Eq, Ord, Show)

data RationalD = RationalD
    { rNumerator :: Integer
    , rDenominator :: Integer
    }
    deriving stock (Eq, Ord, Show)

data AssetClass = AssetClass
    { acSymbol :: PlutusTx.BuiltinByteString
    , acName :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Ord, Show)

newtype Value = Value
    { getValue :: Map PlutusTx.BuiltinByteString (Map PlutusTx.BuiltinByteString Integer)
    }
    deriving stock (Eq, Show)

instance Arbitrary OutputReference where
    arbitrary = OutputReference <$> arbitraryBuiltinByteString 32 <*> arbitrary

instance Arbitrary Credential where
    arbitrary =
        Gen.oneof
            [ PublicKeyCredential <$> arbitraryBuiltinByteString 28
            , ScriptCredential <$> arbitraryBuiltinByteString 28
            ]

instance Arbitrary StakeCredential where
    arbitrary =
        Gen.oneof
            [ Inline <$> arbitrary
            , Pointer <$> arbitrary <*> arbitrary <*> arbitrary
            ]

instance Arbitrary Address where
    arbitrary = Address <$> arbitrary <*> arbitrary

instance Arbitrary RationalD where
    arbitrary = RationalD <$> arbitrary <*> arbitrary

instance Arbitrary AssetClass where
    arbitrary = AssetClass <$> arbitraryAnyBuiltinByteString <*> arbitraryAnyBuiltinByteString

instance Arbitrary Value where
    arbitrary = do
        policies <- Gen.listOf ((,) <$> arbitraryAnyBuiltinByteString <*> Gen.listOf ((,) <$> arbitraryAnyBuiltinByteString <*> arbitrary))
        pure $ Value $ Map.fromList [(policy, Map.fromList assets) | (policy, assets) <- policies]

instance PTx.ToData OutputReference where
    toBuiltinData OutputReference{orTransactionId, orOutputIndex} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkConstr 0 [PlutusTx.mkB orTransactionId]
            , PlutusTx.mkI orOutputIndex
            ]

instance PTx.FromData OutputReference where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [txHash, D.getI -> Just orOutputIndex]) ->
            D.withConstr txHash $ \case
                (0, [D.getB -> Just orTransactionId]) ->
                    Just OutputReference{orTransactionId, orOutputIndex}
                _ -> Nothing
        _ -> Nothing

instance PTx.ToData Credential where
    toBuiltinData = \case
        PublicKeyCredential hashBytes -> PlutusTx.mkConstr 0 [PlutusTx.mkB hashBytes]
        ScriptCredential hashBytes -> PlutusTx.mkConstr 1 [PlutusTx.mkB hashBytes]

instance PTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just hashValue]) -> pure (PublicKeyCredential hashValue)
        (1, [D.getB -> Just hashValue]) -> pure (ScriptCredential hashValue)
        _ -> Nothing

instance PTx.ToData StakeCredential where
    toBuiltinData = \case
        Inline credential ->
            PlutusTx.mkConstr 0 [PTx.toBuiltinData credential]
        Pointer pointerSlotNumber pointerTransactionIndex pointerCertificateIndex ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.mkConstr
                    0
                    [ PlutusTx.mkI pointerSlotNumber
                    , PlutusTx.mkI pointerTransactionIndex
                    , PlutusTx.mkI pointerCertificateIndex
                    ]
                ]

instance PTx.FromData StakeCredential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [credentialData]) ->
            Inline <$> PTx.fromBuiltinData credentialData
        (1, [pointerData]) ->
            D.withConstr pointerData $ \case
                (0, [D.getI -> Just pointerSlotNumber, D.getI -> Just pointerTransactionIndex, D.getI -> Just pointerCertificateIndex]) ->
                    Just Pointer{pointerSlotNumber, pointerTransactionIndex, pointerCertificateIndex}
                _ -> Nothing
        _ -> Nothing

instance PTx.ToData Address where
    toBuiltinData Address{addressPaymentCredential, addressStakeCredential} =
        PlutusTx.mkConstr
            0
            [ PTx.toBuiltinData addressPaymentCredential
            , maybeToOptionData addressStakeCredential
            ]

instance PTx.FromData Address where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [paymentCredential, stakeCredential]) ->
            Address
                <$> PTx.fromBuiltinData paymentCredential
                <*> maybeFromOptionData PTx.fromBuiltinData stakeCredential
        _ -> Nothing

instance PTx.ToData RationalD where
    toBuiltinData RationalD{rNumerator, rDenominator} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI rNumerator, PlutusTx.mkI rDenominator]

instance PTx.FromData RationalD where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just rNumerator, D.getI -> Just rDenominator]) ->
            Just RationalD{rNumerator, rDenominator}
        _ -> Nothing

instance PTx.ToData AssetClass where
    toBuiltinData AssetClass{acSymbol, acName} =
        PlutusTx.mkConstr 0 [PlutusTx.mkB acSymbol, PlutusTx.mkB acName]

instance PTx.FromData AssetClass where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just acSymbol, D.getB -> Just acName]) ->
            Just AssetClass{acSymbol, acName}
        _ -> Nothing

instance PTx.ToData Value where
    toBuiltinData (Value valueMap) =
        PlutusTx.mkMap
            [ ( PlutusTx.mkB policyId
              , PlutusTx.mkMap
                    [ (PlutusTx.mkB assetName, PlutusTx.mkI quantity)
                    | (assetName, quantity) <- Map.toAscList assets
                    ]
              )
            | (policyId, assets) <- Map.toAscList valueMap
            ]

instance PTx.FromData Value where
    fromBuiltinData dt = do
        policyEntries <- fromMap dt
        valueMap <-
            traverse
                ( \(policyData, assetsData) -> do
                    policyId <- D.getB policyData
                    assetEntries <- fromMap assetsData
                    assets <-
                        traverse
                            (\(assetNameData, quantityData) -> do
                                assetName <- D.getB assetNameData
                                quantity <- D.getI quantityData
                                pure (assetName, quantity))
                            assetEntries
                    pure (policyId, Map.fromList assets)
                )
                policyEntries
        pure $ Value (Map.fromList valueMap)

arbitraryBuiltinByteString :: Int -> QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary

arbitraryAnyBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryAnyBuiltinByteString = do
    size <- Gen.chooseInt (0, 64)
    arbitraryBuiltinByteString size

fromMap :: PlutusTx.BuiltinData -> Maybe [(PlutusTx.BuiltinData, PlutusTx.BuiltinData)]
fromMap dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        Just
        (const Nothing)
        (const Nothing)
        (const Nothing)

maybeToOptionData :: PTx.ToData a => Maybe a -> PlutusTx.BuiltinData
maybeToOptionData = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [PTx.toBuiltinData value]

maybeFromOptionData :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
maybeFromOptionData decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing
