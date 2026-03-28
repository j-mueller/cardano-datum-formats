{-# LANGUAGE TemplateHaskell #-}

module Cardano.Protocol.Strike.Settings (
    SettingsDatum (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.JSON (jsonOptions)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (deriveTypeScript)
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import GHC.Generics (Generic)
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
    deriving stock (Eq, Show, Generic)

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

instance ToJSON SettingsDatum where
    toJSON = Aeson.genericToJSON (jsonOptions 2)
    toEncoding = Aeson.genericToEncoding (jsonOptions 2)

instance FromJSON SettingsDatum where
    parseJSON = Aeson.genericParseJSON (jsonOptions 2)

$(deriveTypeScript (jsonOptions 2) ''SettingsDatum)

instance Schema.ToSchema SettingsDatum where
    declareNamedSchema = Schema.genericDeclareNamedSchema (SchemaOptions.fromAesonOptions (jsonOptions 2))
