module Cardano.Protocol.Pulse.Test (
    tests,
) where

import Cardano.Address.Aiken qualified as Aiken
import Cardano.Api qualified as C
import Cardano.Data qualified as Datum
import Cardano.Protocol.Pulse.Common qualified as Common
import Cardano.Protocol.Pulse.Market qualified as Market
import Cardano.Protocol.Pulse.MarketInfo qualified as MarketInfo
import Cardano.Protocol.Pulse.Oracle qualified as Oracle
import Cardano.Protocol.Pulse.Order qualified as Order
import Cardano.Protocol.Pulse.SYVault qualified as SYVault
import Cardano.Protocol.Pulse.YTStake qualified as YTStake
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Cardano.Transaction.OutputReference qualified as OutputReference
import PlutusTx.Builtins qualified as PlutusTx
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "pulse"
        [ testGroup
            "common"
            [ testProperty "Aiken Credential" (datumRoundTrip @Aiken.Credential)
            , testProperty "Aiken StakeCredential" (datumRoundTrip @Aiken.StakeCredential)
            , testProperty "Aiken Address" (datumRoundTrip @Aiken.AikenAddress)
            , testProperty "OutputReference" (datumRoundTrip @OutputReference.OutputReference)
            , testProperty "PubKeyHash" (datumRoundTrip @Common.PubKeyHash)
            ]
        , testGroup
            "datum roundtrips"
            [ testProperty "MarketInfoDatum" (datumRoundTrip @MarketInfo.MarketInfoDatum)
            , testProperty "MarketDatum" (datumRoundTrip @Market.MarketDatum)
            , testProperty "SYVaultDatum" (datumRoundTrip @SYVault.SYVaultDatum)
            , testProperty "OracleDatum" (datumRoundTrip @Oracle.OracleDatum)
            , testProperty "YTStakeDatum" (datumRoundTrip @YTStake.YTStakeDatum)
            , testProperty "OrderDatum" (datumRoundTrip @Order.OrderDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode market info datum" decodeMarketInfoDatum
            , HUnit.testCase "decode market datum" decodeMarketDatum
            , HUnit.testCase "decode sy vault datum" decodeSYVaultDatum
            , HUnit.testCase "decode oracle datum" decodeOracleDatum
            , HUnit.testCase "decode yt stake datum" decodeYTStakeDatum
            , HUnit.testCase "decode order datum" decodeOrderDatum
            ]
        ]

decodeMarketInfoDatum :: HUnit.Assertion
decodeMarketInfoDatum = do
    let marketInfoHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ffd8799f581c91206651a11c64ad878968150d8b2d8b970dff46208c66b42a9df90a4c534e454b5f504552505f4c50ff1903e8530014df1050542d534e454b5f504552505f4c50530014df1059542d534e454b5f504552505f4c501b0000019c8c0a8aa01b00000001cf7c5800192710581c399b00074539d5c39c9d622e9afcfd1756e92f4ade0910df57a8bd39581cc3e0b44426e524ce86b5b1ae23c975aa7ae12bacf6b52dfd727c02d0581cfebe2c972dfbc6d4deb5a2a21409a2c5397fa440ff9307303d2d218a581c916d2114608f04cdf910f2e6655cf76da3c2e95d8a2bd8997dc32b35581cff0048fdba12c321bfc42d2ee9925d0d380ed26aa9f9bb2b0f30b2a1581c0c3b71e6468510341c813626cfa4d008d0b0c50d6ffcea188878e865d8799f581c14cfea925344e8f1c96985c8c1cf2e403e759b6d4113d52b1f996bf9530014df104c502d534e454b5f504552505f4c50ffd8799f581c95ff8558aeecf27e50871032069d250fce16561d820739bd6a7276f7581b535452494b455f4c505f534e454b5f334d5f30305f4f5241434c45ff1a004c4b4019cf3719373e1864194c2c18321901f4190bb8ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @MarketInfo.MarketInfoDatum marketInfoHex)
    HUnit.assertEqual "min liquidity" 1000 (MarketInfo.midMinLiquidity datum)
    HUnit.assertEqual "pt token name" (hexToBuiltinByteString "0014df1050542d534e454b5f504552505f4c50") (MarketInfo.midPtTokenName datum)
    HUnit.assertEqual "order script hash" (hexToBuiltinByteString "c3e0b44426e524ce86b5b1ae23c975aa7ae12bacf6b52dfd727c02d0") (MarketInfo.midOrderScriptHash datum)
    HUnit.assertEqual "sy script hash" (hexToBuiltinByteString "febe2c972dfbc6d4deb5a2a21409a2c5397fa440ff9307303d2d218a") (MarketInfo.midSyScriptHash datum)
    HUnit.assertEqual "yt stake script hash" (hexToBuiltinByteString "916d2114608f04cdf910f2e6655cf76da3c2e95d8a2bd8997dc32b35") (MarketInfo.midYtStakeScriptHash datum)

decodeMarketDatum :: HUnit.Assertion
decodeMarketDatum = do
    let marketHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ff1a0005c7251a00040eba1a0003536c1903e8ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Market.MarketDatum marketHex)
    HUnit.assertEqual "total sy" 378661 (Market.mdTotalSy datum)
    HUnit.assertEqual "total pt" 265914 (Market.mdTotalPt datum)
    HUnit.assertEqual "total lp" 217964 (Market.mdTotalLp datum)
    HUnit.assertEqual "reserved lp" 1000 (Market.mdReservedLp datum)

decodeSYVaultDatum :: HUnit.Assertion
decodeSYVaultDatum = do
    let syVaultHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ff1a00052257ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @SYVault.SYVaultDatum syVaultHex)
    HUnit.assertEqual "market tx id" (hexToBuiltinByteString "1348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb") (OutputReference.orTransactionId $ SYVault.svdMarketId datum)
    HUnit.assertEqual "market output index" 0 (OutputReference.orOutputIndex $ SYVault.svdMarketId datum)
    HUnit.assertEqual "total sy" 336471 (SYVault.svdTotalSy datum)

decodeOracleDatum :: HUnit.Assertion
decodeOracleDatum = do
    let oracleHex =
            "d8799f1950101927101b0000019adc8e7493ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Oracle.OracleDatum oracleHex)
    HUnit.assertEqual "py index" 20496 (Oracle.odPyIndex datum)
    HUnit.assertEqual "base" 10000 (Oracle.odBased datum)
    HUnit.assertEqual "updated at" 1764636914835 (Oracle.odUpdatedAt datum)

decodeYTStakeDatum :: HUnit.Assertion
decodeYTStakeDatum = do
    let ytStakeHex =
            "d8799fd8799f58201348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb00ffd8799f5820c11d8e6afde8c9b0d81ee76025d56d8dcf33acbc69a329ff45042fb169db078b00ff581c233c9e7bc10c8bd5104f4a8c8eca171a14904a3452877e6ae204dea60000195010ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @YTStake.YTStakeDatum ytStakeHex)
    HUnit.assertEqual "market tx id" (hexToBuiltinByteString "1348ace5f4c75b56140c368d674d484291fff3c7604b06d96ed80501ed693fbb") (OutputReference.orTransactionId $ YTStake.ysdMarketId datum)
    HUnit.assertEqual "market output index" 0 (OutputReference.orOutputIndex $ YTStake.ysdMarketId datum)
    HUnit.assertEqual "stake tx id" (hexToBuiltinByteString "c11d8e6afde8c9b0d81ee76025d56d8dcf33acbc69a329ff45042fb169db078b") (OutputReference.orTransactionId $ YTStake.ysdStakeId datum)
    HUnit.assertEqual "stake output index" 0 (OutputReference.orOutputIndex $ YTStake.ysdStakeId datum)
    HUnit.assertEqual "owner pkh" (hexToBuiltinByteString "233c9e7bc10c8bd5104f4a8c8eca171a14904a3452877e6ae204dea6") (PlutusTx.toBuiltin $ C.serialiseToRawBytes $ Common.getPubKeyHash $ YTStake.ysdOwnerPkh datum)
    HUnit.assertEqual "staked yt amount" 0 (YTStake.ysdStakedYtAmount datum)
    HUnit.assertEqual "unclaimed sy amount" 0 (YTStake.ysdUnclaimedSyAmount datum)
    HUnit.assertEqual "py index" 20496 (YTStake.ysdPyIndex datum)

decodeOrderDatum :: HUnit.Assertion
decodeOrderDatum = do
    let orderHex =
            "d87c9f581cd4c35258ed54c3527e0078fa33871e986db11854d9cb4f779d89e604d8799fd8799f581cd4c35258ed54c3527e0078fa33871e986db11854d9cb4f779d89e604ffd8799fd8799fd8799f581ccf9eaf5aa407e4b84e1dfe67fe5eae41c1c5a17de12335354f391120ffffffffd87a80ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Order.OrderDatum orderHex)
    case datum of
        Order.OMintLP{} -> pure ()
        _ -> HUnit.assertFailure "expected OMintLP"
