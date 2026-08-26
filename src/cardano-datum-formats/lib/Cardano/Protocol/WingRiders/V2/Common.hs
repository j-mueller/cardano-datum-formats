{-# LANGUAGE LambdaCase #-}

module Cardano.Protocol.WingRiders.V2.Common (
    Credential (..),
    StakingCredential (..),
    Address (..),
) where

import Cardano.Data qualified as D
import GHC.Generics (Generic)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data Credential
    = VerificationKeyCredential PlutusTx.BuiltinByteString
    | ScriptCredential PlutusTx.BuiltinByteString
    deriving stock (Eq, Show, Generic)

instance Arbitrary Credential where
    arbitrary =
        Gen.oneof
            [ VerificationKeyCredential <$> D.arbitraryBuiltinByteString
            , ScriptCredential <$> D.arbitraryBuiltinByteString
            ]

instance PlutusTx.ToData Credential where
    toBuiltinData = \case
        VerificationKeyCredential keyHash -> PlutusTx.mkConstr 0 [PlutusTx.mkB keyHash]
        ScriptCredential scriptHash -> PlutusTx.mkConstr 1 [PlutusTx.mkB scriptHash]

instance PlutusTx.FromData Credential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just keyHash]) -> Just (VerificationKeyCredential keyHash)
        (1, [D.getB -> Just scriptHash]) -> Just (ScriptCredential scriptHash)
        _ -> Nothing

data StakingCredential
    = StakingHash Credential
    | StakingPointer Integer Integer Integer
    deriving stock (Eq, Show, Generic)

instance Arbitrary StakingCredential where
    arbitrary =
        Gen.oneof
            [ StakingHash <$> arbitrary
            , StakingPointer <$> arbitrary <*> arbitrary <*> arbitrary
            ]

instance PlutusTx.ToData StakingCredential where
    toBuiltinData = \case
        StakingHash credential -> PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData credential]
        StakingPointer slot transactionIndex certificateIndex ->
            PlutusTx.mkConstr
                1
                [ PlutusTx.mkI slot
                , PlutusTx.mkI transactionIndex
                , PlutusTx.mkI certificateIndex
                ]

instance PlutusTx.FromData StakingCredential where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [credential]) -> StakingHash <$> PlutusTx.fromBuiltinData credential
        (1, [D.getI -> Just slot, D.getI -> Just transactionIndex, D.getI -> Just certificateIndex]) ->
            Just (StakingPointer slot transactionIndex certificateIndex)
        _ -> Nothing

data Address = Address
    { addressPaymentCredential :: Credential
    , addressStakingCredential :: Maybe StakingCredential
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary Address where
    arbitrary = Address <$> arbitrary <*> arbitrary

instance PlutusTx.ToData Address where
    toBuiltinData Address{addressPaymentCredential, addressStakingCredential} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.toBuiltinData addressPaymentCredential
            , PlutusTx.toBuiltinData addressStakingCredential
            ]

instance PlutusTx.FromData Address where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [paymentCredential, stakingCredential]) ->
            Address
                <$> PlutusTx.fromBuiltinData paymentCredential
                <*> PlutusTx.fromBuiltinData stakingCredential
        _ -> Nothing
