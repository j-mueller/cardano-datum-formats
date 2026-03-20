module Cardano.Protocol.Indigo.Test (
    tests,
) where

import Cardano.Data qualified as Datum
import Cardano.Protocol.Indigo.CDP qualified as CDP
import Cardano.Protocol.Indigo.Common qualified as Common
import Cardano.Protocol.Indigo.Gov qualified as Gov
import Cardano.Protocol.Indigo.IAsset qualified as IAsset
import Cardano.Protocol.Indigo.InterestOracle qualified as InterestOracle
import Cardano.Protocol.Indigo.LRP qualified as LRP
import Cardano.Protocol.Indigo.PriceOracle qualified as PriceOracle
import Cardano.Protocol.Indigo.StabilityPool qualified as StabilityPool
import Cardano.Protocol.Indigo.Staking qualified as Staking
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "indigo"
        [ testGroup
            "common"
            [ testProperty "OnChainDecimal" (datumRoundTrip @Common.OnChainDecimal)
            , testProperty "AssetClass" (datumRoundTrip @Common.AssetClass)
            , testProperty "OutputReference" (datumRoundTrip @Common.OutputReference)
            , testProperty "Credential" (datumRoundTrip @Common.Credential)
            , testProperty "StakeCredential" (datumRoundTrip @Common.StakeCredential)
            , testProperty "Address" (datumRoundTrip @Common.Address)
            , testProperty "SPInteger" (datumRoundTrip @Common.SPInteger)
            , testProperty "EpochToScaleKey" (datumRoundTrip @Common.EpochToScaleKey)
            ]
        , testGroup
            "datum roundtrips"
            [ testProperty "PriceOracleDatum" (datumRoundTrip @PriceOracle.PriceOracleDatum)
            , testProperty "InterestOracleDatum" (datumRoundTrip @InterestOracle.InterestOracleDatum)
            , testProperty "CDPDatum" (datumRoundTrip @CDP.CDPDatum)
            , testProperty "IAssetDatum" (datumRoundTrip @IAsset.IAssetDatum)
            , testProperty "GovDatum" (datumRoundTrip @Gov.GovDatum)
            , testProperty "LRPDatum" (datumRoundTrip @LRP.LRPDatum)
            , testProperty "StakingDatum" (datumRoundTrip @Staking.StakingDatum)
            , testProperty "StabilityPoolDatum" (datumRoundTrip @StabilityPool.StabilityPoolDatum)
            ]
        , testGroup
            "sdk decode"
            [ HUnit.testCase "decode price oracle datum" decodePriceOracleDatum
            , HUnit.testCase "decode interest oracle datum" decodeInterestOracleDatum
            , HUnit.testCase "decode active cdp datum" decodeActiveCDPDatum
            , HUnit.testCase "decode frozen cdp datum" decodeFrozenCDPDatum
            , HUnit.testCase "decode iasset datum" decodeIAssetDatum
            , HUnit.testCase "decode staking manager datum" decodeStakingManagerDatum
            , HUnit.testCase "decode staking position datum" decodeStakingPositionDatum
            , HUnit.testCase "decode stability pool datum" decodeStabilityPoolDatum
            , HUnit.testCase "decode stability pool account datum" decodeStabilityPoolAccountDatum
            , HUnit.testCase "decode stability pool account adjust datum" decodeStabilityPoolAccountAdjustDatum
            , HUnit.testCase "decode snapshot epoch to scale to sum datum" decodeSnapshotEpochToScaleToSumDatum
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode live cdp datum" decodeLiveCDPDatum
            , HUnit.testCase "decode live staking datum" decodeLiveStakingDatum
            , HUnit.testCase "decode live stability account datum" decodeLiveStabilityAccountDatum
            , HUnit.testCase "decode live lrp datum" decodeLiveLRPDatum
            , HUnit.testCase "decode live gov datum" decodeLiveGovDatum
            ]
        ]

decodePriceOracleDatum :: HUnit.Assertion
decodePriceOracleDatum = do
    let oracleHex = "d8799fd8799f1a0013c347ff1b00000194d68e13d8ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @PriceOracle.PriceOracleDatum oracleHex)
    HUnit.assertEqual "price" (Common.OnChainDecimal 1295175) (PriceOracle.podPrice datum)
    HUnit.assertEqual "expiration" 1738766423000 (PriceOracle.podExpiration datum)

decodeInterestOracleDatum :: HUnit.Assertion
decodeInterestOracleDatum = do
    let oracleHex = "d8799f1b0180e51d1ae19514d8799f1a00030d40ff1b00000194ce33c598ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @InterestOracle.InterestOracleDatum oracleHex)
    HUnit.assertEqual "unitary interest" 108338304224695572 (InterestOracle.iodUnitaryInterest datum)
    HUnit.assertEqual "interest rate" (Common.OnChainDecimal 200000) (InterestOracle.iodInterestRate datum)
    HUnit.assertEqual "last updated" 1738626287000 (InterestOracle.iodLastUpdated datum)

decodeActiveCDPDatum :: HUnit.Assertion
decodeActiveCDPDatum = do
    let cdpHex = "d8799fd8799fd8799f581c98e30e1c6dbb727dc98bdcb48b99b313c97fabfb537ff4b29a94ed1cff44695553441b00000004d9b0a47ed8799f1b00000194d5ebec201b03022de04fddf5f9ffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @CDP.CDPDatum cdpHex)
    HUnit.assertEqual "owner" (Just $ hexToBuiltinByteString "98e30e1c6dbb727dc98bdcb48b99b313c97fabfb537ff4b29a94ed1c") (CDP.ccCdpOwner $ CDP.getCDPContent datum)
    HUnit.assertEqual "asset" (hexToBuiltinByteString "69555344") (CDP.ccIAsset $ CDP.getCDPContent datum)
    HUnit.assertEqual "minted amt" 20832101502 (CDP.ccMintedAmt $ CDP.getCDPContent datum)
    case CDP.ccCdpFees (CDP.getCDPContent datum) of
        CDP.ActiveCDPInterestTracking settled snapshot -> do
            HUnit.assertEqual "last settled" 1738755796000 settled
            HUnit.assertEqual "interest snapshot" 216786173503075833 snapshot
        _ -> HUnit.assertFailure "expected active cdp fees"

decodeFrozenCDPDatum :: HUnit.Assertion
decodeFrozenCDPDatum = do
    let cdpHex = "d8799fd8799fd87a8044695553441a0050924ed87a9f1a0002765a1a0003ca56ffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @CDP.CDPDatum cdpHex)
    HUnit.assertEqual "owner" Nothing (CDP.ccCdpOwner $ CDP.getCDPContent datum)
    HUnit.assertEqual "asset" (hexToBuiltinByteString "69555344") (CDP.ccIAsset $ CDP.getCDPContent datum)
    HUnit.assertEqual "minted amt" 5280334 (CDP.ccMintedAmt $ CDP.getCDPContent datum)
    case CDP.ccCdpFees (CDP.getCDPContent datum) of
        CDP.FrozenCDPAccumulatedFees treasury indy -> do
            HUnit.assertEqual "treasury" 161370 treasury
            HUnit.assertEqual "indy stakers" 248406 indy
        _ -> HUnit.assertFailure "expected frozen cdp fees"

decodeIAssetDatum :: HUnit.Assertion
decodeIAssetDatum = do
    let assetHex = "d87a9fd8799f4469455448d87a9fd8799fd8799f581c6c9497ffd7e8baf86c3c0d6fcd43c524daa49ad5fceba26d715468e952694554483230323231323139313931333032ffffffd8799f581c7b75e317505dddce858ae7bf200656a967c7544e55efa5d18ef302494d694554485f494e544552455354ffd8799f1a08f0d180ffd8799f1a06dac2c0ffd8799f1a068e7780ffd8799f1a000186a0ffd8799f1a001e8480ffd8799f19c350ffd8799f1a000f4240ffd8799f1a000f4240ffd8799f1a01c9c380ffd87980d8799f4469534f4cffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @IAsset.IAssetDatum assetHex)
    let content = IAsset.getIAssetContent datum
    HUnit.assertEqual "asset name" (hexToBuiltinByteString "69455448") (IAsset.iacAssetName content)
    HUnit.assertEqual "first asset" False (IAsset.iacFirstIAsset content)
    HUnit.assertEqual "next asset" (Just $ hexToBuiltinByteString "69534f4c") (IAsset.iacNextIAsset content)

decodeStakingManagerDatum :: HUnit.Assertion
decodeStakingManagerDatum = do
    let stakingHex = "d8799fd8799f1b000009c04704429ed8799f1b000001402802fec1ffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Staking.StakingDatum stakingHex)
    case datum of
        Staking.StakingManager content -> do
            HUnit.assertEqual "total stake" 10721429832350 (Staking.smcTotalStake content)
            HUnit.assertEqual "snapshot ada" 1375060819649 (Staking.rsSnapshotAda $ Staking.smcManagerSnapshot content)
        _ -> HUnit.assertFailure "expected staking manager datum"

decodeStakingPositionDatum :: HUnit.Assertion
decodeStakingPositionDatum = do
    let stakingHex = "d87a9fd8799f581cd45527a088a92fd31f42b5777fe39c40f810e0f79d13c6d77eeb7f43bf1853d8799f1a5c8c1cfb1b0000019616971410ffffd8799f1b0000013a7ed5b0fdffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Staking.StakingDatum stakingHex)
    case datum of
        Staking.StakingPosition content -> do
            HUnit.assertEqual "owner" (hexToBuiltinByteString "d45527a088a92fd31f42b5777fe39c40f810e0f79d13c6d77eeb7f43") (Staking.spcOwner content)
            HUnit.assertEqual "locked amount" (Map.fromList [(83, (1552686331, 1744135722000))]) (Staking.spcLockedAmount content)
            HUnit.assertEqual "snapshot ada" 1350747664637 (Staking.rsSnapshotAda $ Staking.spcPositionSnapshot content)
        _ -> HUnit.assertFailure "expected staking position datum"

decodeStabilityPoolDatum :: HUnit.Assertion
decodeStabilityPoolDatum = do
    let stabilityHex = "d8799fd8799f4469555344d8799fd8799f1b0a37ad5c452ffb2affd8799fc24d1f94ac680ce6b48ea21bb122baffd8799f1b0fde3bba456cd5deff0100ffa2d8799f0000ffd8799f1b084494e2d23b2b7effd8799f0100ffd8799f1b0fde3bba456cd5deffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @StabilityPool.StabilityPoolDatum stabilityHex)
    case datum of
        StabilityPool.StabilityPool content -> do
            HUnit.assertEqual "asset" (hexToBuiltinByteString "69555344") (StabilityPool.spcAsset content)
            HUnit.assertEqual "epoch to scale count" 2 (Map.size $ StabilityPool.spcEpochToScaleToSum content)
        _ -> HUnit.assertFailure "expected stability pool datum"

decodeStabilityPoolAccountDatum :: HUnit.Assertion
decodeStabilityPoolAccountDatum = do
    let stabilityHex = "d87a9fd8799f581c12c646d4c6d7a35c14788d15f0f6142f6148975d8932592fbd625f674469555344d8799fd8799f1b0a37ad5c452ffb2affd8799fc24c39fa2838b1f7dd38267f0a6dffd8799f1b0fde3b75c28ab489ff0100ffd87a80ffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @StabilityPool.StabilityPoolDatum stabilityHex)
    case datum of
        StabilityPool.Account content -> do
            HUnit.assertEqual "owner" (hexToBuiltinByteString "12c646d4c6d7a35c14788d15f0f6142f6148975d8932592fbd625f67") (StabilityPool.acOwner content)
            HUnit.assertEqual "request" Nothing (StabilityPool.acRequest content)
        _ -> HUnit.assertFailure "expected stability account datum"

decodeStabilityPoolAccountAdjustDatum :: HUnit.Assertion
decodeStabilityPoolAccountAdjustDatum = do
    let stabilityHex = "d87a9fd8799f581c90e40129516ee738fa6aa9183cf57b45c46946496e1590d34ca1b15c4469555344d8799fd8799f1b0a374472be304a62ffd8799fc24b01aef07f96e5ce00f80000ffd8799f1b0f88aa07a1048079ff0100ffd8799fd87a9f3a0007c359d8799fd8799f581c90e40129516ee738fa6aa9183cf57b45c46946496e1590d34ca1b15cffd8799fd8799fd8799f581c75a4f9204b9308a92a09b0e22b94125e56f24b73bb85e2795f176c6affffffffffffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @StabilityPool.StabilityPoolDatum stabilityHex)
    case datum of
        StabilityPool.Account content -> do
            case StabilityPool.acRequest content of
                Just (StabilityPool.Adjust amount outputAddress) -> do
                    HUnit.assertEqual "amount" (-508762) amount
                    case Common.addressPaymentCredential outputAddress of
                        Common.PublicKeyCredential hashBytes ->
                            HUnit.assertEqual "payment credential" (hexToBuiltinByteString "90e40129516ee738fa6aa9183cf57b45c46946496e1590d34ca1b15c") hashBytes
                        _ -> HUnit.assertFailure "expected payment pubkey credential"
                _ -> HUnit.assertFailure "expected adjust request"
        _ -> HUnit.assertFailure "expected stability account datum"

decodeSnapshotEpochToScaleToSumDatum :: HUnit.Assertion
decodeSnapshotEpochToScaleToSumDatum = do
    let snapshotHex = "d87b9fd8799f4469555344bfd8799f0000ffd8799f1b084494e2d23b2b7effd8799f0100ffd8799f1b0fde3bba456cd5deffffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @StabilityPool.StabilityPoolDatum snapshotHex)
    case datum of
        StabilityPool.SnapshotEpochToScaleToSum content -> do
            HUnit.assertEqual "asset" (hexToBuiltinByteString "69555344") (StabilityPool.sesAsset content)
            HUnit.assertEqual "snapshot size" 2 (Map.size $ StabilityPool.sesSnapshot content)
        _ -> HUnit.assertFailure "expected snapshot datum"

decodeLiveCDPDatum :: HUnit.Assertion
decodeLiveCDPDatum = do
    let cdpHex = "d8799fd8799fd8799f581c2dbce9a05ff3ecdedbaf202ea14e89a4396560be74f4f5748aa289f6ff446945544810d8799f1b0000019267ed49281b00acaf755bf951d4ffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @CDP.CDPDatum cdpHex)
    HUnit.assertEqual "live asset" (hexToBuiltinByteString "69455448") (CDP.ccIAsset $ CDP.getCDPContent datum)
    HUnit.assertEqual "live minted amt" 16 (CDP.ccMintedAmt $ CDP.getCDPContent datum)

decodeLiveStakingDatum :: HUnit.Assertion
decodeLiveStakingDatum = do
    let stakingHex = "d87a9fd8799f581c9afd6723b0d9b844adae3f3babf99e579305f804dc51c9cd0dace11da11819d8799f1a00ac00831b0000018ae22408d8ffd8799f1b000000d31ac1adc0ffffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Staking.StakingDatum stakingHex)
    case datum of
        Staking.StakingPosition content -> do
            HUnit.assertEqual "live lock count" 1 (Map.size $ Staking.spcLockedAmount content)
            HUnit.assertEqual "live snapshot" 906687000000 (Staking.rsSnapshotAda $ Staking.spcPositionSnapshot content)
        _ -> HUnit.assertFailure "expected live staking position datum"

decodeLiveStabilityAccountDatum :: HUnit.Assertion
decodeLiveStabilityAccountDatum = do
    let stabilityHex = "d87a9fd8799f581c794343926a43d13d72892383b093678fa0602c5ad6338620eb7c302c4469555344d8799fd8799f1b0a374472be304a62ffd8799fc24c0135ca6daccae361e38b4ec5ffd8799f1b0f8897dbd3ada701ff0100ffd87a80ffff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @StabilityPool.StabilityPoolDatum stabilityHex)
    case datum of
        StabilityPool.Account content -> do
            HUnit.assertEqual "request" Nothing (StabilityPool.acRequest content)
            HUnit.assertEqual "asset" (hexToBuiltinByteString "69555344") (StabilityPool.acAsset content)
        _ -> HUnit.assertFailure "expected live stability account datum"

decodeLiveLRPDatum :: HUnit.Assertion
decodeLiveLRPDatum = do
    let lrpHex = "d8799f581c07a678cc14e6db47698094e252044db3014ae3d4008fde18212ee5e04469425443d8799f1b0000001bf08eb000ff1a3b9aca00ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @LRP.LRPDatum lrpHex)
    HUnit.assertEqual "owner" (hexToBuiltinByteString "07a678cc14e6db47698094e252044db3014ae3d4008fde18212ee5e0") (LRP.ldOwner datum)
    HUnit.assertEqual "asset" (hexToBuiltinByteString "69425443") (LRP.ldIAsset datum)
    HUnit.assertEqual "lovelaces" 1000000000 (LRP.ldLovelacesToSpend datum)

decodeLiveGovDatum :: HUnit.Assertion
decodeLiveGovDatum = do
    let govHex = "d8799f1871d8799f1a05f5e1001a19bfcc001a006ddd001a240c8400d8799f00ff1a006ddd00081b000000746a5288001b000000746a5288001b00000068c6171400ff0204001b0000006aa2ed6400ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Gov.GovDatum govHex)
    HUnit.assertEqual "current proposal" 113 (Gov.gdCurrentProposal datum)
    HUnit.assertEqual "current version" 2 (Gov.gdCurrentVersion datum)
    HUnit.assertEqual "iassets count" 4 (Gov.gdIAssetsCount datum)
