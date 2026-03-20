module Cardano.Protocol.Strike.Settings (
    SettingsDatum (..),
) where

import Cardano.Data qualified as D
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))

data SettingsDatum = SettingsDatum
    { sdInterestRate :: Integer
    , sdMaxLeverageFactor :: Integer
    , sdMaxPositionUsdValue :: Integer
    , sdMinPositionUsdValue :: Integer
    , sdMaintainMarginAmount :: Integer
    , sdOpeningFeeBasisPoints :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary SettingsDatum where
    arbitrary = SettingsDatum <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance PTx.ToData SettingsDatum where
    toBuiltinData SettingsDatum{sdInterestRate, sdMaxLeverageFactor, sdMaxPositionUsdValue, sdMinPositionUsdValue, sdMaintainMarginAmount, sdOpeningFeeBasisPoints} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI sdInterestRate
            , PlutusTx.mkI sdMaxLeverageFactor
            , PlutusTx.mkI sdMaxPositionUsdValue
            , PlutusTx.mkI sdMinPositionUsdValue
            , PlutusTx.mkI sdMaintainMarginAmount
            , PlutusTx.mkI sdOpeningFeeBasisPoints
            ]

instance PTx.FromData SettingsDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [ D.getI -> Just sdInterestRate
            , D.getI -> Just sdMaxLeverageFactor
            , D.getI -> Just sdMaxPositionUsdValue
            , D.getI -> Just sdMinPositionUsdValue
            , D.getI -> Just sdMaintainMarginAmount
            , D.getI -> Just sdOpeningFeeBasisPoints
            ]) -> Just SettingsDatum{sdInterestRate, sdMaxLeverageFactor, sdMaxPositionUsdValue, sdMinPositionUsdValue, sdMaintainMarginAmount, sdOpeningFeeBasisPoints}
        _ -> Nothing
