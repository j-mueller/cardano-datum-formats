module Cardano.Protocol.Dano.Transaction (
    SwapPlan (..), prepareSwap, swapBuilder,
) where

import Cardano.Api qualified as C
import Cardano.Asset qualified as Asset
import Cardano.Protocol.Dano.Pool
import Cardano.Protocol.Dano.Swap
import Control.Monad (unless)
import Convex.BuildTx qualified as Build
import GHC.Exts (fromList)
import Data.Maybe (fromMaybe)
import PlutusTx qualified as P
import PlutusTx.Builtins qualified as B

-- The pool output is placed at the index supplied to swapBuilder. References
-- and spending indices are resolved against the final balanced body.
data SwapPlan = SwapPlan
    { swapInput :: C.TxIn
    , swapInputValue :: C.Value
    , swapPoolOutput :: C.TxOut C.CtxTx C.ConwayEra
    , swapScriptReference :: C.TxIn
    , swapConfigReference :: C.TxIn
    , swapScriptHash :: C.ScriptHash
    , swapDelta :: Integer
    , swapReceived :: Integer
    }
    deriving stock (Eq, Show)

prepareSwap :: C.ScriptHash -> C.AssetId -> C.TxIn -> C.TxOut C.CtxUTxO C.ConwayEra -> C.TxIn -> C.TxIn -> ProtocolConfig -> Integer -> Integer -> Integer -> Either String SwapPlan
prepareSwap scriptHash nft input (C.TxOut address amount inline reference) scriptRef configRef config epoch delta minimumOutput = do
    unless (scriptAddressMatches address) $ Left "Dano pool payment credential does not match the deployment"
    datum <- case inline of
        C.TxOutDatumInline _ raw -> maybe (Left "invalid Dano pool datum") Right $ P.fromData $ C.toPlutusData $ C.getScriptData raw
        _ -> Left "Dano pool requires an inline datum"
    x <- maybe (Left "invalid Dano token X") Right $ Asset.toAssetId (unTupleAsset $ tokenX datum)
    y <- maybe (Left "invalid Dano token Y") Right $ Asset.toAssetId (unTupleAsset $ tokenY datum)
    unless (x /= C.AdaAssetId && y /= C.AdaAssetId && x /= y) $ Left "Dano direct execution requires two distinct native assets"
    let value = C.txOutValueToValue amount
        quantity asset = let C.Quantity q = C.selectAsset value asset in q
    unless (case nft of C.AssetId (C.PolicyId policy) _ -> policy == scriptHash && nft /= x && nft /= y && quantity nft == 1; _ -> False) $ Left "Dano pool authentication NFT is missing"
    unless (epoch >= lastWithdrawEpoch datum && minimumOutput > 0 && circulatingLpToken datum > 0) $ Left "invalid Dano swap parameters"
    (received, platformFee) <- maybe (Left "Dano quote violates pool bounds") Right $ quoteExactIn datum config (quantity x) (quantity y) 0 delta
    unless (received >= minimumOutput) $ Left "Dano output is below the route minimum"
    let updated = datum
            { platformFeeX = platformFeeX datum + if delta > 0 then platformFee else 0
            , platformFeeY = platformFeeY datum + if delta < 0 then platformFee else 0
            , totalSwapFee = totalSwapFee datum + swapFee config
            , lastWithdrawEpoch = epoch
            }
        (offered,receivedAsset) = if delta > 0 then (x,y) else (y,x)
        nextValue = value <> fromList [(offered,C.Quantity $ abs delta),(receivedAsset,C.Quantity $ negate received),(C.AdaAssetId,C.Quantity $ swapFee config)]
        nextDatum = C.unsafeHashableScriptData . C.fromPlutusData $ P.toData updated
        output = C.TxOut address (C.TxOutValueShelleyBased C.ShelleyBasedEraConway $ C.toMaryValue nextValue) (C.TxOutDatumInline C.BabbageEraOnwardsConway nextDatum) reference
    pure $ SwapPlan input value output scriptRef configRef scriptHash delta received
  where
    scriptAddressMatches (C.AddressInEra _ (C.ShelleyAddress _ payment _)) = C.fromShelleyPaymentCredential payment == C.PaymentCredentialByScript scriptHash
    scriptAddressMatches _ = False

swapBuilder :: C.NetworkId -> Int -> SwapPlan -> Build.TxBuilder C.ConwayEra
swapBuilder network outputIndex plan = Build.execBuildTx $ do
    Build.addReference (swapConfigReference plan)
    Build.spendPlutusRefWithInlineDatumWithRedeemerFn (swapInput plan) (swapScriptReference plan) C.PlutusScriptV3 (redeemer False)
    Build.addWithdrawalWithTxBody rewardAddress 0 $ \body ->
        C.ScriptWitness C.ScriptWitnessForStakeAddr $ Build.buildRefScriptWitness (swapScriptReference plan) C.PlutusScriptV3 C.NoScriptDatumForStake (redeemer True body)
  where
    rewardAddress = C.makeStakeAddress network (C.StakeCredentialByScript $ swapScriptHash plan)
    redeemer withdrawal body =
        let inputIndex = toInteger $ Build.findIndexSpending (swapInput plan) body
            context = if withdrawal then toInteger $ Build.findIndexReference (swapConfigReference plan) body else inputIndex
         in fromMaybe (B.mkB "") $ encodeSwapRedeemer $ SwapRedeemer context [SwapEntry inputIndex (toInteger outputIndex) (swapDelta plan)]
