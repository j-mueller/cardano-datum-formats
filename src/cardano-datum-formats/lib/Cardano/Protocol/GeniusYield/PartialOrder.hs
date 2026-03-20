module Cardano.Protocol.GeniusYield.PartialOrder (
    PartialOrderConfigDatum (..),
    PartialOrderContainedFee (..),
    PartialOrderDatum (..),
    PartialOrderFeeOutput (..),
) where

import Cardano.Data qualified as D
import Cardano.Protocol.GeniusYield.Common (
    Address,
    AssetClass,
    OutputReference,
    RationalD,
    Value,
    arbitraryAnyBuiltinByteString,
    maybeFromOptionData,
    maybeToOptionData,
 )
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import PlutusTx qualified as PTx
import PlutusTx.Builtins qualified as PlutusTx
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Gen qualified as Gen

data PartialOrderConfigDatum = PartialOrderConfigDatum
    { pocdSignatories :: [PlutusTx.BuiltinByteString]
    , pocdReqSignatores :: Integer
    , pocdNftSymbol :: PlutusTx.BuiltinByteString
    , pocdFeeAddr :: Address
    , pocdMakerFeeFlat :: Integer
    , pocdMakerFeeRatio :: RationalD
    , pocdTakerFee :: Integer
    , pocdMinDeposit :: Integer
    }
    deriving stock (Eq, Show)

data PartialOrderContainedFee = PartialOrderContainedFee
    { pocfLovelaces :: Integer
    , pocfOfferedTokens :: Integer
    , pocfAskedTokens :: Integer
    }
    deriving stock (Eq, Show)

data PartialOrderFeeOutput = PartialOrderFeeOutput
    { pofdMentionedFees :: Map OutputReference Value
    , pofdReservedValue :: Value
    , pofdSpentUTxORef :: Maybe OutputReference
    }
    deriving stock (Eq, Show)

data PartialOrderDatum = PartialOrderDatum
    { podOwnerKey :: PlutusTx.BuiltinByteString
    , podOwnerAddr :: Address
    , podOfferedAsset :: AssetClass
    , podOfferedOriginalAmount :: Integer
    , podOfferedAmount :: Integer
    , podAskedAsset :: AssetClass
    , podPrice :: RationalD
    , podNFT :: PlutusTx.BuiltinByteString
    , podStart :: Maybe Integer
    , podEnd :: Maybe Integer
    , podPartialFills :: Integer
    , podMakerLovelaceFlatFee :: Integer
    , podTakerLovelaceFlatFee :: Integer
    , podContainedFee :: PartialOrderContainedFee
    , podContainedPayment :: Integer
    }
    deriving stock (Eq, Show)

instance Arbitrary PartialOrderConfigDatum where
    arbitrary =
        PartialOrderConfigDatum
            <$> Gen.listOf (arbitraryBuiltinByteString 28)
            <*> arbitrary
            <*> arbitraryAnyBuiltinByteString
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance Arbitrary PartialOrderContainedFee where
    arbitrary = PartialOrderContainedFee <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary PartialOrderFeeOutput where
    arbitrary = do
        entries <- Gen.listOf ((,) <$> arbitrary <*> arbitrary)
        PartialOrderFeeOutput <$> pure (Map.fromList entries) <*> arbitrary <*> arbitrary

instance Arbitrary PartialOrderDatum where
    arbitrary =
        PartialOrderDatum
            <$> arbitraryBuiltinByteString 28
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitraryAnyBuiltinByteString
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary
            <*> arbitrary

instance PTx.ToData PartialOrderConfigDatum where
    toBuiltinData
        PartialOrderConfigDatum
            { pocdSignatories
            , pocdReqSignatores
            , pocdNftSymbol
            , pocdFeeAddr
            , pocdMakerFeeFlat
            , pocdMakerFeeRatio
            , pocdTakerFee
            , pocdMinDeposit
            } =
            PlutusTx.mkConstr
                0
                [ PlutusTx.mkList [PlutusTx.mkB signatory | signatory <- pocdSignatories]
                , PlutusTx.mkI pocdReqSignatores
                , PlutusTx.mkB pocdNftSymbol
                , PTx.toBuiltinData pocdFeeAddr
                , PlutusTx.mkI pocdMakerFeeFlat
                , PTx.toBuiltinData pocdMakerFeeRatio
                , PlutusTx.mkI pocdTakerFee
                , PlutusTx.mkI pocdMinDeposit
                ]

instance PTx.FromData PartialOrderConfigDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [signatories, D.getI -> Just pocdReqSignatores, D.getB -> Just pocdNftSymbol, feeAddr, D.getI -> Just pocdMakerFeeFlat, makerFeeRatio, D.getI -> Just pocdTakerFee, D.getI -> Just pocdMinDeposit]) -> do
            signatoryData <- fromList signatories
            pocdSignatories <- traverse D.getB signatoryData
            PartialOrderConfigDatum
                <$> pure pocdSignatories
                <*> pure pocdReqSignatores
                <*> pure pocdNftSymbol
                <*> PTx.fromBuiltinData feeAddr
                <*> pure pocdMakerFeeFlat
                <*> PTx.fromBuiltinData makerFeeRatio
                <*> pure pocdTakerFee
                <*> pure pocdMinDeposit
        _ -> Nothing

instance PTx.ToData PartialOrderContainedFee where
    toBuiltinData PartialOrderContainedFee{pocfLovelaces, pocfOfferedTokens, pocfAskedTokens} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkI pocfLovelaces
            , PlutusTx.mkI pocfOfferedTokens
            , PlutusTx.mkI pocfAskedTokens
            ]

instance PTx.FromData PartialOrderContainedFee where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [D.getI -> Just pocfLovelaces, D.getI -> Just pocfOfferedTokens, D.getI -> Just pocfAskedTokens]) ->
            Just PartialOrderContainedFee{pocfLovelaces, pocfOfferedTokens, pocfAskedTokens}
        _ -> Nothing

instance PTx.ToData PartialOrderFeeOutput where
    toBuiltinData PartialOrderFeeOutput{pofdMentionedFees, pofdReservedValue, pofdSpentUTxORef} =
        PlutusTx.mkConstr
            0
            [ PlutusTx.mkMap [(PTx.toBuiltinData outRef, PTx.toBuiltinData value) | (outRef, value) <- Map.toAscList pofdMentionedFees]
            , PTx.toBuiltinData pofdReservedValue
            , maybeToOptionData pofdSpentUTxORef
            ]

instance PTx.FromData PartialOrderFeeOutput where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [mentionedFees, reservedValue, spentUTxORef]) -> do
            entries <- fromMap mentionedFees
            decodedEntries <-
                traverse
                    (\(outRefData, valueData) -> do
                        outRef <- PTx.fromBuiltinData outRefData
                        value <- PTx.fromBuiltinData valueData
                        pure (outRef, value))
                    entries
            PartialOrderFeeOutput
                <$> pure (Map.fromList decodedEntries)
                <*> PTx.fromBuiltinData reservedValue
                <*> maybeFromOptionData PTx.fromBuiltinData spentUTxORef
        _ -> Nothing

instance PTx.ToData PartialOrderDatum where
    toBuiltinData
        PartialOrderDatum
            { podOwnerKey
            , podOwnerAddr
            , podOfferedAsset
            , podOfferedOriginalAmount
            , podOfferedAmount
            , podAskedAsset
            , podPrice
            , podNFT
            , podStart
            , podEnd
            , podPartialFills
            , podMakerLovelaceFlatFee
            , podTakerLovelaceFlatFee
            , podContainedFee
            , podContainedPayment
            } =
            PlutusTx.mkConstr
                0
                [ PlutusTx.mkB podOwnerKey
                , PTx.toBuiltinData podOwnerAddr
                , PTx.toBuiltinData podOfferedAsset
                , PlutusTx.mkI podOfferedOriginalAmount
                , PlutusTx.mkI podOfferedAmount
                , PTx.toBuiltinData podAskedAsset
                , PTx.toBuiltinData podPrice
                , PlutusTx.mkB podNFT
                , maybeToOptionData podStart
                , maybeToOptionData podEnd
                , PlutusTx.mkI podPartialFills
                , PlutusTx.mkI podMakerLovelaceFlatFee
                , PlutusTx.mkI podTakerLovelaceFlatFee
                , PTx.toBuiltinData podContainedFee
                , PlutusTx.mkI podContainedPayment
                ]

instance PTx.FromData PartialOrderDatum where
    fromBuiltinData dt = D.withConstr dt $ \case
        (0, [ D.getB -> Just podOwnerKey
            , ownerAddr
            , offeredAsset
            , D.getI -> Just podOfferedOriginalAmount
            , D.getI -> Just podOfferedAmount
            , askedAsset
            , price
            , D.getB -> Just podNFT
            , start
            , end
            , D.getI -> Just podPartialFills
            , D.getI -> Just podMakerLovelaceFlatFee
            , D.getI -> Just podTakerLovelaceFlatFee
            , containedFee
            , D.getI -> Just podContainedPayment
            ]) ->
                PartialOrderDatum
                    <$> pure podOwnerKey
                    <*> PTx.fromBuiltinData ownerAddr
                    <*> PTx.fromBuiltinData offeredAsset
                    <*> pure podOfferedOriginalAmount
                    <*> pure podOfferedAmount
                    <*> PTx.fromBuiltinData askedAsset
                    <*> PTx.fromBuiltinData price
                    <*> pure podNFT
                    <*> maybeFromOptionData D.getI start
                    <*> maybeFromOptionData D.getI end
                    <*> pure podPartialFills
                    <*> pure podMakerLovelaceFlatFee
                    <*> pure podTakerLovelaceFlatFee
                    <*> PTx.fromBuiltinData containedFee
                    <*> pure podContainedPayment
        _ -> Nothing

fromMap :: PlutusTx.BuiltinData -> Maybe [(PlutusTx.BuiltinData, PlutusTx.BuiltinData)]
fromMap dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        Just
        (const Nothing)
        (const Nothing)
        (const Nothing)

fromList :: PlutusTx.BuiltinData -> Maybe [PlutusTx.BuiltinData]
fromList dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        Just
        (const Nothing)
        (const Nothing)

arbitraryBuiltinByteString :: Int -> Gen.Gen PlutusTx.BuiltinByteString
arbitraryBuiltinByteString size =
    PlutusTx.toBuiltin . BS.pack <$> Gen.vectorOf size arbitrary
