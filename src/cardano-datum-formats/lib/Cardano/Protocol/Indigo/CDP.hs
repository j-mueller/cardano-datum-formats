module Cardano.Protocol.Indigo.CDP (
    CDPDatum (..),
    CDPFees (..),
    CDPContent (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.Indigo.Common (arbitraryAnyBuiltinByteString, maybeFromOptionData, maybeToOptionData)
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data CDPFees
    = ActiveCDPInterestTracking
        { cdpfLastSettled :: Integer
        , cdpfUnitaryInterestSnapshot :: Integer
        }
    | FrozenCDPAccumulatedFees
        { cdpfLovelacesTreasury :: Integer
        , cdpfLovelacesIndyStakers :: Integer
        }
    deriving stock (Eq, Show)

data CDPContent = CDPContent
    { ccCdpOwner :: Maybe PlutusTx.BuiltinByteString
    , ccIAsset :: PlutusTx.BuiltinByteString
    , ccMintedAmt :: Integer
    , ccCdpFees :: CDPFees
    }
    deriving stock (Eq, Show)

newtype CDPDatum = CDPDatum
    { getCDPContent :: CDPContent
    }
    deriving stock (Eq, Show)

instance Arbitrary CDPFees where
    arbitrary =
        Gen.oneof
            [ ActiveCDPInterestTracking <$> arbitrary <*> arbitrary
            , FrozenCDPAccumulatedFees <$> arbitrary <*> arbitrary
            ]

instance Arbitrary CDPContent where
    arbitrary =
        CDPContent
            <$> arbitraryMaybeBytes
            <*> arbitraryAnyBuiltinByteString
            <*> arbitrary
            <*> arbitrary

instance Arbitrary CDPDatum where
    arbitrary = CDPDatum <$> arbitrary

instance PTx.ToData CDPFees where
    toBuiltinData = \case
        ActiveCDPInterestTracking cdpfLastSettled cdpfUnitaryInterestSnapshot ->
            PlutusTx.mkConstr 0 [PlutusTx.mkI cdpfLastSettled, PlutusTx.mkI cdpfUnitaryInterestSnapshot]
        FrozenCDPAccumulatedFees cdpfLovelacesTreasury cdpfLovelacesIndyStakers ->
            PlutusTx.mkConstr 1 [PlutusTx.mkI cdpfLovelacesTreasury, PlutusTx.mkI cdpfLovelacesIndyStakers]

instance PTx.FromData CDPFees where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just cdpfLastSettled, D.getI -> Just cdpfUnitaryInterestSnapshot]) ->
            Just ActiveCDPInterestTracking{cdpfLastSettled, cdpfUnitaryInterestSnapshot}
        (1, [D.getI -> Just cdpfLovelacesTreasury, D.getI -> Just cdpfLovelacesIndyStakers]) ->
            Just FrozenCDPAccumulatedFees{cdpfLovelacesTreasury, cdpfLovelacesIndyStakers}
        _ -> Nothing

instance PTx.ToData CDPContent where
    toBuiltinData CDPContent{ccCdpOwner, ccIAsset, ccMintedAmt, ccCdpFees} =
        PlutusTx.mkConstr
            0
            [ maybeToOptionDataBytes ccCdpOwner
            , PlutusTx.mkB ccIAsset
            , PlutusTx.mkI ccMintedAmt
            , PTx.toBuiltinData ccCdpFees
            ]

instance PTx.FromData CDPContent where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [owner, D.getB -> Just ccIAsset, D.getI -> Just ccMintedAmt, cdpFees]) ->
            CDPContent
                <$> maybeFromOptionData D.getB owner
                <*> pure ccIAsset
                <*> pure ccMintedAmt
                <*> PTx.fromBuiltinData cdpFees
        _ -> Nothing

instance PTx.ToData CDPDatum where
    toBuiltinData (CDPDatum content) =
        PlutusTx.mkConstr 0 [cdpContentToVariantData content]

instance PTx.FromData CDPDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [content]) -> CDPDatum <$> cdpContentFromVariantData content
        _ -> Nothing

arbitraryMaybeBytes :: Gen.Gen (Maybe PlutusTx.BuiltinByteString)
arbitraryMaybeBytes = Gen.oneof [pure Nothing, Just <$> arbitraryAnyBuiltinByteString]

cdpContentToVariantData :: CDPContent -> PlutusTx.BuiltinData
cdpContentToVariantData CDPContent{ccCdpOwner, ccIAsset, ccMintedAmt, ccCdpFees} =
    PlutusTx.mkConstr
        0
        [ maybeToOptionDataBytes ccCdpOwner
        , PlutusTx.mkB ccIAsset
        , PlutusTx.mkI ccMintedAmt
        , PTx.toBuiltinData ccCdpFees
        ]

cdpContentFromVariantData :: PlutusTx.BuiltinData -> Maybe CDPContent
cdpContentFromVariantData dt = D.withConstr dt $ \case
    (0, [owner, D.getB -> Just ccIAsset, D.getI -> Just ccMintedAmt, cdpFees]) ->
        CDPContent
            <$> maybeFromOptionData D.getB owner
            <*> pure ccIAsset
            <*> pure ccMintedAmt
            <*> PTx.fromBuiltinData cdpFees
    _ -> Nothing

maybeToOptionDataBytes :: Maybe PlutusTx.BuiltinByteString -> PlutusTx.BuiltinData
maybeToOptionDataBytes = maybeToOptionData . fmap BytesAsData

newtype BytesAsData = BytesAsData { unBytesAsData :: PlutusTx.BuiltinByteString }

instance PTx.ToData BytesAsData where
    toBuiltinData = PlutusTx.mkB . unBytesAsData
