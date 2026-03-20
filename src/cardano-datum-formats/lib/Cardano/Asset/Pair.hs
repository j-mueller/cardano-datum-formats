{-# LANGUAGE NamedFieldPuns #-}

module Cardano.Asset.Pair (
    Pair,
    pair,
    asset1,
    asset2,
    lpAssetName,
) where

import Cardano.Api (AssetId (..))
import Cardano.Api qualified as C
import Data.ByteString (ByteString)
import PlutusCore.Crypto.Hash qualified as Hash
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen
import Test.QuickCheck.Hedgehog qualified as H

data Pair = Pair
    { pAsset1 :: AssetId
    , pAsset2 :: AssetId
    }
    deriving stock (Eq, Ord, Show)

instance Arbitrary Pair where
    arbitrary = do
        a1 <- H.hedgehog Gen.genAssetId
        a2 <- H.hedgehog Gen.genAssetId `Gen.suchThat` (/= a1)
        pure (pair a1 a2)

pair :: AssetId -> AssetId -> Pair
pair C.AdaAssetId a2 = Pair C.AdaAssetId a2
pair a1 C.AdaAssetId = Pair C.AdaAssetId a1
pair a1 a2 = Pair (min a1 a2) (max a1 a2)

asset1 :: Pair -> AssetId
asset1 = pAsset1

asset2 :: Pair -> AssetId
asset2 = pAsset2

assetIdBs :: AssetId -> ByteString
assetIdBs = \case
    AdaAssetId -> mempty
    AssetId policy tokenName ->
        C.serialiseToRawBytes policy <> C.serialiseToRawBytes tokenName

lpAssetName :: Pair -> C.AssetName
lpAssetName Pair{pAsset1, pAsset2} =
    let b1 = Hash.sha3_256 (assetIdBs pAsset1)
        b2 = Hash.sha3_256 (assetIdBs pAsset2)
     in C.UnsafeAssetName $ Hash.sha3_256 (b1 <> b2)
