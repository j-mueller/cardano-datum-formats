module Cardano.Protocol.Strike.Common (
    AddressHash (..),
    OpenPositionType (..),
    PositionSide (..),
    ScriptHash (..),
    arbitraryAnyBuiltinByteString,
    arbitraryBuiltinByteString,
    maybeFromOptionData,
    maybeToOptionData,
) where

import Cardano.Data qualified as D
import Data.ByteString qualified as BS
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

newtype AddressHash = AddressHash
    { getAddressHash :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show)

newtype ScriptHash = ScriptHash
    { getScriptHash :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show)

data PositionSide
    = Long
    | Short
    deriving stock (Eq, Show)

data OpenPositionType
    = MarketOrder
    | LimitOrder
    deriving stock (Eq, Show)

instance Arbitrary AddressHash where
    arbitrary = AddressHash <$> arbitraryBuiltinByteString 28

instance Arbitrary ScriptHash where
    arbitrary = ScriptHash <$> arbitraryBuiltinByteString 28

instance Arbitrary PositionSide where
    arbitrary = Gen.elements [Long, Short]

instance Arbitrary OpenPositionType where
    arbitrary = Gen.elements [MarketOrder, LimitOrder]

instance PTx.ToData AddressHash where
    toBuiltinData = PlutusTx.mkB . getAddressHash

instance PTx.FromData AddressHash where
    fromBuiltinData dt = AddressHash <$> D.getB dt

instance PTx.ToData ScriptHash where
    toBuiltinData = PlutusTx.mkB . getScriptHash

instance PTx.FromData ScriptHash where
    fromBuiltinData dt = ScriptHash <$> D.getB dt

instance PTx.ToData PositionSide where
    toBuiltinData = \case
        Long -> PlutusTx.mkConstr 0 []
        Short -> PlutusTx.mkConstr 1 []

instance PTx.FromData PositionSide where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just Long
        (1, []) -> Just Short
        _ -> Nothing

instance PTx.ToData OpenPositionType where
    toBuiltinData = \case
        MarketOrder -> PlutusTx.mkConstr 0 []
        LimitOrder -> PlutusTx.mkConstr 1 []

instance PTx.FromData OpenPositionType where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, []) -> Just MarketOrder
        (1, []) -> Just LimitOrder
        _ -> Nothing

arbitraryBuiltinByteString :: Int -> QC.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary

arbitraryAnyBuiltinByteString :: QC.Gen PlutusTx.BuiltinByteString
arbitraryAnyBuiltinByteString = do
    size <- Gen.chooseInt (0, 64)
    arbitraryBuiltinByteString size

maybeToOptionData :: PTx.ToData a => Maybe a -> PlutusTx.BuiltinData
maybeToOptionData = \case
    Nothing -> PlutusTx.mkConstr 1 []
    Just value -> PlutusTx.mkConstr 0 [PTx.toBuiltinData value]

maybeFromOptionData :: (PlutusTx.BuiltinData -> Maybe a) -> PlutusTx.BuiltinData -> Maybe (Maybe a)
maybeFromOptionData decode dt = D.withConstr dt $ \case
    (0, [value]) -> Just <$> decode value
    (1, []) -> Just Nothing
    _ -> Nothing
