module Cardano.Transaction.OutputReference (
    OutputReference (..),
) where

import Cardano.Data qualified as D
import Data.ByteString qualified as BS
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data OutputReference = OutputReference
    { orTransactionId :: PlutusTx.BuiltinByteString
    , orOutputIndex :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary OutputReference where
    arbitrary =
        OutputReference
            <$> arbitraryByteString 32
            <*> arbitrary

instance PlutusTx.ToData OutputReference where
    toBuiltinData OutputReference{orTransactionId, orOutputIndex} =
        PlutusTx.mkConstr 0 [PlutusTx.mkB orTransactionId, PlutusTx.mkI orOutputIndex]

instance PlutusTx.FromData OutputReference where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getB -> Just orTransactionId, D.getI -> Just orOutputIndex]) ->
            Just OutputReference{orTransactionId, orOutputIndex}
        _ -> Nothing

arbitraryByteString :: Int -> QC.Gen PlutusTx.BuiltinByteString
arbitraryByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary
