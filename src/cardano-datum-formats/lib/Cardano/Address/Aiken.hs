module Cardano.Address.Aiken (
    Credential (..),
    StakeCredential (..),
    AikenAddress (..),
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Control.Monad ((>=>))
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog qualified as H

data Credential
    = VerificationKeyCredential (C.Hash C.PaymentKey)
    | ScriptCredential C.ScriptHash
    deriving stock (Eq, Show)

instance Arbitrary Credential where
    arbitrary =
        Gen.oneof
            [ VerificationKeyCredential <$> H.hedgehog (Gen.genVerificationKeyHash C.AsPaymentKey)
            , ScriptCredential <$> H.hedgehog Gen.genScriptHash
            ]

instance PlutusTx.ToData Credential where
    toBuiltinData = \case
        VerificationKeyCredential keyHash -> PlutusTx.mkConstr 0 [D.serialiseHash keyHash]
        ScriptCredential scriptHash -> PlutusTx.mkConstr 1 [D.serialiseScriptHash scriptHash]

instance PlutusTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB >=> D.deserialiseHash -> Just keyHash]) -> Just (VerificationKeyCredential keyHash)
        (1, [D.getB >=> D.deserialiseScriptHash -> Just scriptHash]) -> Just (ScriptCredential scriptHash)
        _ -> Nothing

data StakeCredential
    = Inline Credential
    | Pointer
        { pointerSlotNumber :: Integer
        , pointerTransactionIndex :: Integer
        , pointerCertificateIndex :: Integer
        }
    deriving stock (Eq, Show)

instance Arbitrary StakeCredential where
    arbitrary =
        Gen.oneof
            [ Inline <$> arbitrary
            , Pointer <$> arbitrary <*> arbitrary <*> arbitrary
            ]

instance PlutusTx.ToData StakeCredential where
    toBuiltinData = \case
        Inline credential -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData credential]
        Pointer pointerSlotNumber pointerTransactionIndex pointerCertificateIndex ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.mkI pointerSlotNumber
                , PlutusTx.mkI pointerTransactionIndex
                , PlutusTx.mkI pointerCertificateIndex
                ]

instance PlutusTx.FromData StakeCredential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [credential]) -> Inline <$> PlutusTx.fromBuiltinData credential
        (1, [D.getI -> Just pointerSlotNumber, D.getI -> Just pointerTransactionIndex, D.getI -> Just pointerCertificateIndex]) ->
            Just Pointer{pointerSlotNumber, pointerTransactionIndex, pointerCertificateIndex}
        _ -> Nothing

data AikenAddress = AikenAddress
    { aaPaymentCredential :: Credential
    , aaStakeCredential :: Maybe StakeCredential
    }
    deriving stock (Eq, Show)

instance Arbitrary AikenAddress where
    arbitrary =
        AikenAddress <$> arbitrary <*> arbitrary

instance PlutusTx.ToData AikenAddress where
    toBuiltinData AikenAddress{aaPaymentCredential, aaStakeCredential} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData aaPaymentCredential
            , maybeToOption aaStakeCredential
            ]

instance PlutusTx.FromData AikenAddress where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [paymentCredential, stakeCredential]) ->
            AikenAddress
                <$> PlutusTx.fromBuiltinData paymentCredential
                <*> fromOption PlutusTx.fromBuiltinData stakeCredential
        _ -> Nothing

maybeToOption :: PlutusTx.ToData a => Maybe a -> PlutusTx.BuiltinData
maybeToOption = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData value]

fromOption :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
fromOption decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing
