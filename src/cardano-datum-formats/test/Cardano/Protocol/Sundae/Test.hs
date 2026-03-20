module Cardano.Protocol.Sundae.Test (
    tests,
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as Datum
import Cardano.Protocol.Sundae.Order qualified as Order
import Cardano.Test.Utils (datumRoundTrip, fromHexRawBytes, hexToBuiltinByteString, scriptDataFromHex)
import Data.Maybe (isJust)
import Data.Proxy (Proxy (..))
import PlutusTx qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "sundae"
        [ testGroup
            "order datum"
            [ testProperty "MultisigScript" (datumRoundTrip @Order.MultisigScript)
            , testProperty "Credential" (datumRoundTrip @Order.Credential)
            , testProperty "StakingCredential" (datumRoundTrip @Order.StakingCredential)
            , testProperty "SundaeAddress" (datumRoundTrip @Order.SundaeAddress)
            , testProperty "SundaeDatum" (datumRoundTrip @Order.SundaeDatum)
            , testProperty "Destination" (datumRoundTrip @Order.Destination)
            , testProperty "StrategyAuthorization" (datumRoundTrip @Order.StrategyAuthorization)
            , testProperty "AssetAmount" (datumRoundTrip @Order.AssetAmount)
            , testProperty "Order" (datumRoundTrip @Order.Order)
            , testProperty "SundaeOrderDatum" (datumRoundTrip @Order.SundaeOrderDatum)
            , HUnit.testCase "decode swap datum from sdk vector" decodeSwapDatum
            , HUnit.testCase "decode valid order datum from sdk vector" decodeValidOrder
            ]
        ]

decodeSwapDatum :: HUnit.Assertion
decodeSwapDatum = do
    let expectedHex =
            "d8799fd8799f581ca933477ea168013e2b5af4a9e029e36d26738eb6dfe382e1f3eab3e2ffd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ff1a000f4240d8799fd8799fd8799f581cc279a3fb3b4e62bbc78e288783b58045d4ae82a18867d8352d02775affd8799fd8799fd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ffffffffd87980ffd87a9f9f40401864ff9f581cfa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a3515351834574494e44591864ffff43d87980ff"
    orderDatum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Order.SundaeOrderDatum expectedHex)
    HUnit.assertEqual
        "pool ident"
        (Just $ hexToBuiltinByteString "a933477ea168013e2b5af4a9e029e36d26738eb6dfe382e1f3eab3e2")
        (Order.soPoolIdent orderDatum)
    HUnit.assertEqual
        "owner"
        (Order.Signature $ mkPaymentKeyHash "121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0")
        (Order.soOwner orderDatum)
    HUnit.assertEqual "max protocol fee" (C.Quantity 1_000_000) (Order.soMaxProtocolFee orderDatum)
    HUnit.assertEqual
        "destination"
        (Order.FixedDestination fixedAddress Order.NoDatum)
        (Order.soDestination orderDatum)
    HUnit.assertEqual
        "details"
        ( Order.Swap
            (Order.AssetAmount C.AdaAssetId (C.Quantity 100))
            ( Order.AssetAmount
                (C.AssetId (mkPolicyId "fa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a351535183") (mkAssetName "74494e4459"))
                (C.Quantity 100)
            )
        )
        (Order.soDetails orderDatum)
    HUnit.assertEqual "extension" (hexToBuiltinByteString "d87980") (Order.soExtension orderDatum)
  where
    fixedAddress =
        Order.SundaeAddress
            (Order.VerificationKeyCredential $ mkPaymentKeyHash "c279a3fb3b4e62bbc78e288783b58045d4ae82a18867d8352d02775a")
            (Just $ Order.Inline $ Order.VerificationKeyCredential $ mkPaymentKeyHash "121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0")

decodeValidOrder :: HUnit.Assertion
decodeValidOrder = do
    let vectorFromSdk =
            "d8799fd8799f581ca933477ea168013e2b5af4a9e029e36d26738eb6dfe382e1f3eab3e2ffd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ff1a000f4240d8799fd8799fd8799f581cc279a3fb3b4e62bbc78e288783b58045d4ae82a18867d8352d02775affd8799fd8799fd8799f581c121fd22e0b57ac206fefc763f8bfa0771919f5218b40691eea4514d0ffffffffd87980ffd87a9f9f40401864ff9f581cfa3eff2047fdf9293c5feef4dc85ce58097ea1c6da4845a3515351834574494e44591864ffff43d87980ff"
    scriptData <-
        either
            HUnit.assertFailure
            pure
            (scriptDataFromHex vectorFromSdk)
    let decoded = PlutusTx.fromBuiltinData @Order.SundaeOrderDatum $ PlutusTx.toBuiltinData $ PlutusTx.dataToBuiltinData $ C.toPlutusData scriptData
    HUnit.assertBool "should decode Sundae order datum from sdk vector" (isJust decoded)

mkPaymentKeyHash :: String -> C.Hash C.PaymentKey
mkPaymentKeyHash hex = fromHexRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.PaymentKey)) hex

mkPolicyId :: String -> C.PolicyId
mkPolicyId hex = fromHexRawBytes C.AsPolicyId hex

mkAssetName :: String -> C.AssetName
mkAssetName hex = fromHexRawBytes C.AsAssetName hex
