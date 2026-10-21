module Cardano.Protocol.Dano.Emulator (tests) where

import Cardano.Api qualified as C
import Cardano.Asset qualified as Asset
import Cardano.Protocol.Dano.Pool
import Cardano.Protocol.Dano.Transaction
import Cardano.Test.Utils
import Control.Monad (unless, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Convex.BuildTx qualified as Build
import Convex.Class (getUtxo, setUtxo)
import Convex.CoinSelection (ChangeOutputPosition (TrailingChange))
import Convex.MockChain (MockchainIO)
import Convex.MockChain.CoinSelection (tryBalanceAndSubmit)
import Convex.MockChain.Defaults qualified as Defaults
import Convex.MockChain.Utils (mockchainSucceeds, mockchainFails)
import Convex.Wallet qualified as Wallet
import Convex.Wallet.MockWallet qualified as Wallet
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict qualified as Map
import Data.List (isInfixOf)
import GHC.Exts (fromList)
import PlutusTx qualified as P
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, assertBool)

tests :: TestTree
tests = testGroup "Dano deployed-validator emulator"
    [ testCase "X to Y passes ledger validation" $ mockchainSucceeds (swapTest False False)
    , testCase "Y to X passes ledger validation" $ mockchainSucceeds (swapTest True False)
    , testCase "excess output fails validator" $ mockchainFails (swapTest False True) (\err -> assertBool ("expected phase-2 script rejection: " <> show err) ("ScriptExecutionErr" `isInfixOf` show err))
    ]

swapTest :: Bool -> Bool -> MockchainIO C.ConwayEra ()
swapTest reverseDirection steal = do
    raw <- liftIO $ BS.readFile "test/data/dano-mainnet-validator.cbor"
    bytes <- either fail pure $ Base16.decode (BS.filter (/= 10) raw)
    script <- either (fail . show) pure $ C.deserialiseFromCBOR (C.AsPlutusScript C.AsPlutusScriptV3) bytes
    let hash = C.hashScript (C.PlutusScript C.PlutusScriptV3 script)
        expected = fromHexRawBytes C.AsScriptHash "d8b69fc53637bcfadbc4469083f706bc293f4d9d2296646c5ca167bb"
    unless (hash == expected) $ fail "Dano fixture script hash mismatch"
    let walletAddress = Wallet.addressInEra Defaults.networkId Wallet.w1
        scriptAddress sh = C.makeShelleyAddressInEra C.ShelleyBasedEraConway Defaults.networkId (C.PaymentCredentialByScript sh) C.NoStakeAddress
        policy = fromHexRawBytes C.AsPolicyId "c48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad"
        x = C.AssetId policy (fromHexRawBytes C.AsAssetName "58")
        y = C.AssetId policy (fromHexRawBytes C.AsAssetName "59")
        nft = C.AssetId (C.PolicyId hash) (fromHexRawBytes C.AsAssetName "d847ecbb34165da6b59bed76287b384c6dea95bc4c626f9bd2b9d88116cd0a23")
        configNft = C.AssetId (fromHexRawBytes C.AsPolicyId "bc2d3b7cf1009c788b1daf2208a38026ab20d7f8209d896ad1a09c14") (fromHexRawBytes C.AsAssetName "72f9dae79bb2dfa39bd8fadf089ea0c8377b5eaa3caef0549396668aef1f9187")
        configHash = fromHexRawBytes C.AsScriptHash "fa991bc2f9c4206e72d713bc3487a72e7901057cabb8d364bebeef8f"
        datum = PoolDatum (TupleAsset $ Asset.fromAssetId x) (TupleAsset $ Asset.fromAssetId y) 10 0 0 0 (Price 99 100) (Price 101 100) 100 100 1000000000 0
        config = ProtocolConfig 1000 100000
        inline d = C.TxOutDatumInline C.BabbageEraOnwardsConway (C.unsafeHashableScriptData $ C.fromPlutusData $ P.toData d)
        value v = C.TxOutValueShelleyBased C.ShelleyBasedEraConway (C.toMaryValue v)
        poolOut = C.TxOut (scriptAddress hash) (value $ fromList [(C.AdaAssetId,3000000),(x,1000000000),(y,1000000000),(nft,1)]) (inline datum) C.ReferenceScriptNone
        refTx = fromHexRawBytes C.AsTxId "1111111111111111111111111111111111111111111111111111111111111111"
        poolInput = C.TxIn refTx (C.TxIx 0)
        scriptRef = C.TxIn refTx (C.TxIx 1)
        configRef = C.TxIn refTx (C.TxIx 2)
        tokenInput = C.TxIn refTx (C.TxIx 3)
        refOut = C.TxOut (scriptAddress hash) (value $ fromList [(C.AdaAssetId,60000000)]) C.TxOutDatumNone (C.ReferenceScript C.BabbageEraOnwardsConway (C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV3) (C.PlutusScript C.PlutusScriptV3 script)))
        configOut = C.TxOut (scriptAddress configHash) (value $ fromList [(C.AdaAssetId,3000000),(configNft,1)]) (inline config) C.ReferenceScriptNone
        tokenOut = C.TxOut walletAddress (value $ fromList [(C.AdaAssetId,10000000),(x,10000000),(y,10000000)]) C.TxOutDatumNone C.ReferenceScriptNone
    existing <- getUtxo
    let seeded = C.toLedgerUTxO C.ShelleyBasedEraConway $ C.UTxO $ Map.fromList [(poolInput,poolOut),(scriptRef,refOut),(configRef,configOut),(tokenInput,tokenOut)]
    setUtxo (existing <> seeded)
    register <- runExceptT $ tryBalanceAndSubmit mempty Wallet.w1 (Build.execBuildTx $ Build.addConwayRegCertNoWitness $ C.StakeCredentialByScript hash) TrailingChange []
    void $ either (fail . show) pure register
    plan <- either fail pure $ prepareSwap hash nft poolInput poolOut scriptRef configRef config 312 (if reverseDirection then -1000000 else 1000000) 1
    let C.TxOut addr amount outDatum reference = swapPoolOutput plan
        receivedAsset = if reverseDirection then x else y
        stolen = C.TxOut addr (value $ C.txOutValueToValue amount <> fromList [(receivedAsset,-100000)]) outDatum reference
        output = if steal then stolen else swapPoolOutput plan
        builder = swapBuilder Defaults.networkId 0 plan <> Build.execBuildTx (do
            Build.addOutput output
            Build.addValidityRangeSlots 0 300
            Build.setMinAdaDepositAll Defaults.bundledProtocolParameters)
    result <- runExceptT $ tryBalanceAndSubmit mempty Wallet.w1 builder TrailingChange []
    void $ either (fail . show) pure result
