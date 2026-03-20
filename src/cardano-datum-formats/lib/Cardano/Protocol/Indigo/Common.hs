module Cardano.Protocol.Indigo.Common (
    Address (..),
    AssetClass (..),
    Credential (..),
    EpochToScaleKey (..),
    OnChainDecimal (..),
    OutputReference (..),
    SPInteger (..),
    StakeCredential (..),
    arbitraryAnyBuiltinByteString,
    arbitraryBuiltinByteString,
    fromMap,
    fromList,
    maybeFromOptionData,
    maybeToOptionData,
) where

import Cardano.Data qualified as D
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

newtype OnChainDecimal = OnChainDecimal
    { getOnChainInt :: Integer
    }
    deriving stock (Eq, Show)

data AssetClass = AssetClass
    { acCurrencySymbol :: PlutusTx.BuiltinByteString
    , acTokenName :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Ord, Show)

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

newtype SPInteger = SPInteger
    { spiValue :: Integer
    }
    deriving stock (Eq, Ord, Show)

data EpochToScaleKey = EpochToScaleKey
    { eskEpoch :: Integer
    , eskScale :: Integer
    }
    deriving stock (Eq, Ord, Show)

instance Arbitrary OnChainDecimal where
    arbitrary = OnChainDecimal <$> arbitrary

instance Arbitrary AssetClass where
    arbitrary = AssetClass <$> arbitraryAnyBuiltinByteString <*> arbitraryAnyBuiltinByteString

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

instance Arbitrary SPInteger where
    arbitrary = SPInteger <$> arbitrary

instance Arbitrary EpochToScaleKey where
    arbitrary = EpochToScaleKey <$> arbitrary <*> arbitrary

instance PTx.ToData OnChainDecimal where
    toBuiltinData (OnChainDecimal value) = PlutusTx.mkConstr 0 [PlutusTx.mkI value]

instance PTx.FromData OnChainDecimal where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just getOnChainInt]) -> Just OnChainDecimal{getOnChainInt}
        _ -> Nothing

instance PTx.ToData AssetClass where
    toBuiltinData AssetClass{acCurrencySymbol, acTokenName} =
        PlutusTx.mkConstr 0 [PlutusTx.mkB acCurrencySymbol, PlutusTx.mkB acTokenName]

instance PTx.FromData AssetClass where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just acCurrencySymbol, D.getB -> Just acTokenName]) ->
            Just AssetClass{acCurrencySymbol, acTokenName}
        _ -> Nothing

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
        PublicKeyCredential hashBytes ->
            PlutusTx.mkConstr 0 [PlutusTx.mkB hashBytes]
        ScriptCredential hashBytes ->
            PlutusTx.mkConstr 1 [PlutusTx.mkB hashBytes]

instance PTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just hashValue]) ->
            pure (PublicKeyCredential hashValue)
        (1, [D.getB -> Just hashValue]) ->
            pure (ScriptCredential hashValue)
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
        (1, [pointerData]) -> do
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

instance PTx.ToData SPInteger where
    toBuiltinData (SPInteger value) = PlutusTx.mkConstr 0 [PlutusTx.mkI value]

instance PTx.FromData SPInteger where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just spiValue]) -> Just SPInteger{spiValue}
        _ -> Nothing

instance PTx.ToData EpochToScaleKey where
    toBuiltinData EpochToScaleKey{eskEpoch, eskScale} =
        PlutusTx.mkConstr 0 [PlutusTx.mkI eskEpoch, PlutusTx.mkI eskScale]

instance PTx.FromData EpochToScaleKey where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just eskEpoch, D.getI -> Just eskScale]) ->
            Just EpochToScaleKey{eskEpoch, eskScale}
        _ -> Nothing

arbitraryBuiltinByteString :: Int -> QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary

arbitraryAnyBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryAnyBuiltinByteString = do
    size <- Gen.chooseInt (0, 64)
    arbitraryBuiltinByteString size

fromList :: PlutusTx.BuiltinData -> Maybe [PlutusTx.BuiltinData]
fromList dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        Just
        (const Nothing)
        (const Nothing)

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
