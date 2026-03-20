module Cardano.Asset (
    Asset (..),
    fromAssetId,
    toAssetId,
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as D
import Data.ByteString qualified as BS
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Hedgehog qualified as H

data Asset = Asset
    { aPolicyId :: PlutusTx.BuiltinByteString
    , aTokenName :: PlutusTx.BuiltinByteString
    }
    deriving stock (Eq, Show)

instance PlutusTx.ToData Asset where
    toBuiltinData Asset{aPolicyId, aTokenName} =
        PlutusTx.mkConstr 0 [PlutusTx.mkB aPolicyId, PlutusTx.mkB aTokenName]

instance PlutusTx.FromData Asset where
    fromBuiltinData k = D.withConstr k $ \case
        (0, [D.getB -> Just aPolicyId, D.getB -> Just aTokenName]) -> Just Asset{aPolicyId, aTokenName}
        _ -> Nothing

fromAssetId :: C.AssetId -> Asset
fromAssetId = \case
    C.AdaAssetId -> Asset (PlutusTx.toBuiltin BS.empty) (PlutusTx.toBuiltin BS.empty)
    C.AssetId policy assetName ->
        Asset
            (PlutusTx.toBuiltin $ C.serialiseToRawBytes policy)
            (PlutusTx.toBuiltin $ C.serialiseToRawBytes assetName)

toAssetId :: Asset -> Maybe C.AssetId
toAssetId Asset{aPolicyId, aTokenName}
    | BS.null (PlutusTx.fromBuiltin aPolicyId) && BS.null (PlutusTx.fromBuiltin aTokenName) = Just C.AdaAssetId
    | otherwise =
        C.AssetId
            <$> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsPolicyId (PlutusTx.fromBuiltin aPolicyId))
            <*> either (const Nothing) Just (C.deserialiseFromRawBytes C.AsAssetName (PlutusTx.fromBuiltin aTokenName))

instance Arbitrary Asset where
    arbitrary = fromAssetId <$> H.hedgehog Gen.genAssetId
