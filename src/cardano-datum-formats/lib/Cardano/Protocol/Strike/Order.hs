module Cardano.Protocol.Strike.Order (
    OrderAction (..),
    OrderDatum (..),
) where

import Cardano.Asset (Asset)
import Cardano.Data qualified as D
import Cardano.Protocol.Strike.Common (
    AddressHash,
    OpenPositionType,
    arbitraryAnyBuiltinByteString,
    maybeFromOptionData,
    maybeToOptionData,
 )
import Cardano.Protocol.Strike.Position (PositionDatum)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data OrderAction
    = OpenPositionOrder
        { oaPositionDatum :: PositionDatum
        , oaOpenPositionType :: OpenPositionType
        }
    | ClosePositionOrder
        { oaOwnerPkh :: AddressHash
        , oaOwnerStakeKey :: Maybe AddressHash
        , oaSendAsset :: Asset
        , oaSendAssetAmount :: Integer
        , oaPoolAssetProfitLoss :: Integer
        , oaPositionPolicyId :: PlutusTx.BuiltinByteString
        , oaBorrowedAmount :: Integer
        }
    | LiquidatePositionOrder
        { oaProfit :: Integer
        , oaLendedAmount :: Integer
        , oaPositionPolicyId :: PlutusTx.BuiltinByteString
        }
    | ProvideLiquidityOrder
        { oaOwnerPkh :: AddressHash
        , oaOwnerStakeKey :: Maybe AddressHash
        , oaLiquidityAsset :: Asset
        }
    | WithdrawLiquidityOrder
        { oaOwnerPkh :: AddressHash
        , oaOwnerStakeKey :: Maybe AddressHash
        }
    deriving stock (Eq, Show)

newtype OrderDatum = OrderDatum
    { odAction :: OrderAction
    }
    deriving stock (Eq, Show)

instance Arbitrary OrderAction where
    arbitrary =
        Gen.oneof
            [ OpenPositionOrder <$> arbitrary <*> arbitrary
            , ClosePositionOrder <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitraryAnyBuiltinByteString <*> arbitrary
            , LiquidatePositionOrder <$> arbitrary <*> arbitrary <*> arbitraryAnyBuiltinByteString
            , ProvideLiquidityOrder <$> arbitrary <*> arbitrary <*> arbitrary
            , WithdrawLiquidityOrder <$> arbitrary <*> arbitrary
            ]

instance Arbitrary OrderDatum where
    arbitrary = OrderDatum <$> arbitrary

instance PTx.ToData OrderAction where
    toBuiltinData = \case
        OpenPositionOrder oaPositionDatum oaOpenPositionType ->
            PlutusTx.mkConstr 0 [PTx.toBuiltinData oaPositionDatum, PTx.toBuiltinData oaOpenPositionType]
        ClosePositionOrder oaOwnerPkh oaOwnerStakeKey oaSendAsset oaSendAssetAmount oaPoolAssetProfitLoss oaPositionPolicyId oaBorrowedAmount ->
            PlutusTx.mkConstr
                1
                [ PTx.toBuiltinData oaOwnerPkh
                , maybeToOptionData oaOwnerStakeKey
                , PTx.toBuiltinData oaSendAsset
                , PlutusTx.mkI oaSendAssetAmount
                , PlutusTx.mkI oaPoolAssetProfitLoss
                , PlutusTx.mkB oaPositionPolicyId
                , PlutusTx.mkI oaBorrowedAmount
                ]
        LiquidatePositionOrder oaProfit oaLendedAmount oaPositionPolicyId ->
            PlutusTx.mkConstr 2 [PlutusTx.mkI oaProfit, PlutusTx.mkI oaLendedAmount, PlutusTx.mkB oaPositionPolicyId]
        ProvideLiquidityOrder oaOwnerPkh oaOwnerStakeKey oaLiquidityAsset ->
            PlutusTx.mkConstr 3 [PTx.toBuiltinData oaOwnerPkh, maybeToOptionData oaOwnerStakeKey, PTx.toBuiltinData oaLiquidityAsset]
        WithdrawLiquidityOrder oaOwnerPkh oaOwnerStakeKey ->
            PlutusTx.mkConstr 4 [PTx.toBuiltinData oaOwnerPkh, maybeToOptionData oaOwnerStakeKey]

instance PTx.FromData OrderAction where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [positionDatum, openPositionType]) ->
            OpenPositionOrder
                <$> PTx.fromBuiltinData positionDatum
                <*> PTx.fromBuiltinData openPositionType
        (1, [ownerPkh, ownerStakeKey, sendAsset, D.getI -> Just oaSendAssetAmount, D.getI -> Just oaPoolAssetProfitLoss, D.getB -> Just oaPositionPolicyId, D.getI -> Just oaBorrowedAmount]) ->
            ClosePositionOrder
                <$> PTx.fromBuiltinData ownerPkh
                <*> maybeFromOptionData PTx.fromBuiltinData ownerStakeKey
                <*> PTx.fromBuiltinData sendAsset
                <*> pure oaSendAssetAmount
                <*> pure oaPoolAssetProfitLoss
                <*> pure oaPositionPolicyId
                <*> pure oaBorrowedAmount
        (2, [D.getI -> Just oaProfit, D.getI -> Just oaLendedAmount, D.getB -> Just oaPositionPolicyId]) ->
            pure LiquidatePositionOrder{oaProfit, oaLendedAmount, oaPositionPolicyId}
        (3, [ownerPkh, ownerStakeKey, liquidityAsset]) ->
            ProvideLiquidityOrder
                <$> PTx.fromBuiltinData ownerPkh
                <*> maybeFromOptionData PTx.fromBuiltinData ownerStakeKey
                <*> PTx.fromBuiltinData liquidityAsset
        (4, [ownerPkh, ownerStakeKey]) ->
            WithdrawLiquidityOrder
                <$> PTx.fromBuiltinData ownerPkh
                <*> maybeFromOptionData PTx.fromBuiltinData ownerStakeKey
        _ -> Nothing

instance PTx.ToData OrderDatum where
    toBuiltinData OrderDatum{odAction} = PlutusTx.mkConstr 0 [PTx.toBuiltinData odAction]

instance PTx.FromData OrderDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [action]) -> OrderDatum <$> PTx.fromBuiltinData action
        _ -> Nothing
