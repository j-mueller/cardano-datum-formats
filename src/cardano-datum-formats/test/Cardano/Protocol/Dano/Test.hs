module Cardano.Protocol.Dano.Test (tests) where

import Cardano.Protocol.Dano.Emulator qualified as Emulator
import Cardano.Protocol.Dano.Pool
import Cardano.Protocol.Dano.Swap
import Cardano.Asset (Asset (..))
import Cardano.Data qualified as Datum
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck qualified as QC
import PlutusTx.Builtins qualified as B

-- SDK vectors: dano-finance/clmm-sdk commit 0416dc1f3531ce77eec7664ea3947f192a3aca40.
tests :: TestTree
tests = testGroup "Dano CLMM"
    [ Emulator.tests
    , QC.testProperty "pool datum roundtrip" (datumRoundTrip @PoolDatum)
    , QC.testProperty "protocol configuration roundtrip" (datumRoundTrip @ProtocolConfig)
    , QC.testProperty "asset tuple roundtrip" (datumRoundTrip @TupleAsset)
    , testCase "mainnet USDM/USDCx datum" $ do
        d <- either assertFailure pure $ Datum.fromBuiltinDataHex @PoolDatum "d8799f9f581cc48cbb3d5e57ed56e276bc45f99ab39abe94e6cd7ac39fb402da47ad480014df105553444dff9f581c1f3aec8bfe7ea4fe14c5f121e2a92e301afe414147860d557cac7e34455553444378ff0a1a05df37ab1a05d068cb1a0d2182e0d8799f1b0023427a28ab6d731b002386f26fc10000ffd8799f1b0023cae7bcde280e1b002386f26fc10000ff186418641b0000001494ddcdd1190292ff"
        lpFeeRate d @?= 10
        sqrtLowerPrice d @?= Price 9924716620639603 10000000000000000
        minXChange d @?= 100
        totalSwapFee d @?= 220300000
        lastWithdrawEpoch d @?= 658
    , testCase "preprod protocol configuration" $ do
        d <- either assertFailure pure $ Datum.fromBuiltinDataHex @ProtocolConfig "d8799f1903e81a000186a0ff"
        d @?= ProtocolConfig 1000 100000
    , testCase "both swap directions match SDK" $ mapM_ checkVector vectors
    , testCase "rejects exhausted reserves and minimum violations" $ do
        quoteExactIn pool config 1000000 1000000 0 1000000000 @?= Nothing
        quoteExactIn pool{minXChange=100000} config 1000000 1000000 0 99999 @?= Nothing
        quoteExactIn pool config 1000000 1000000 0 0 @?= Nothing
        virtualReserves (Price 1 0) (Price 1 1) 100 100 @?= Nothing
        virtualReserves (Price 2 1) (Price 1 1) 100 100 @?= Nothing
    , testCase "reserve accounting excludes platform fees and ADA deposit" $ do
        activeReserves pool{platformFeeX=10,platformFeeY=20} 1000 2000 0 @?= Just (990,1980)
        let ada = TupleAsset $ Asset ("") ("")
        activeReserves pool{tokenX=ada,platformFeeX=10,totalSwapFee=100000} 4000000 2000 30 @?= Just (900020,2000)
    , testCase "packed signed swap redeemers match SDK byte layout" $ do
        let positive = SwapRedeemer 2 [SwapEntry 3 1 1]
            negative = SwapRedeemer 0 [SwapEntry 1 0 (-1)]
        encodeSwapRedeemer positive @?= Just (B.mkB $ hexToBuiltinByteString "020303010000000000000000000000000000000000000000000000000000000000000001")
        encodeSwapRedeemer negative @?= Just (B.mkB $ hexToBuiltinByteString "00030100ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff")
        (encodeSwapRedeemer negative >>= decodeSwapRedeemer) @?= Just negative
        let multiple = SwapRedeemer 255 [SwapEntry 0 255 (2^(255::Int)-1),SwapEntry 255 0 (negate $ 2^(255::Int))]
        (encodeSwapRedeemer multiple >>= decodeSwapRedeemer) @?= Just multiple
        encodeSwapRedeemer (SwapRedeemer 0 [SwapEntry 0 0 (negate (2^(255::Int))-1)]) @?= Nothing
        encodeSwapRedeemer (SwapRedeemer 0 [SwapEntry 0 0 0]) @?= Nothing
        encodeSwapRedeemer (SwapRedeemer 256 [SwapEntry 0 0 1]) @?= Nothing
        encodeSwapRedeemer (SwapRedeemer 0 [SwapEntry 0 0 (2^(255::Int))]) @?= Nothing
        decodeSwapRedeemer (B.mkB $ hexToBuiltinByteString "000301") @?= Nothing
    ]
  where
    config = ProtocolConfig 1000 100000
    pool = PoolDatum (TupleAsset $ Asset (hexToBuiltinByteString "01") (hexToBuiltinByteString "02")) (TupleAsset $ Asset (hexToBuiltinByteString "03") (hexToBuiltinByteString "04")) 10 0 0 0 (Price 99 100) (Price 101 100) 0 0 1000 0
    checkVector (x,y,input,output,fee) = quoteExactIn pool config x y 0 input @?= Just (output,fee)
    vectors = [(1000000000,1000000000,100000,99890,10),
        (1000000000,1000000000,-100000,99909,10),
        (1000000000,1000000000,1000000,998891,100),
        (1000000000,1000000000,-1000000,999088,100),
        (1000000000,1000000000,10000000,9988018,1000),
        (1000000000,1000000000,-10000000,9989996,1000),
        (5000000000,100000000,100000,97989,10),
        (5000000000,100000000,-100000,101847,10),
        (5000000000,100000000,1000000,979891,100),
        (5000000000,100000000,-1000000,1018472,100),
        (5000000000,100000000,10000000,9798575,1000),
        (5000000000,100000000,-10000000,10184366,1000),
        (100000000,5000000000,100000,101827,10),
        (100000000,5000000000,-100000,98009,10),
        (100000000,5000000000,1000000,1018269,100),
        (100000000,5000000000,-1000000,980087,100),
        (100000000,5000000000,10000000,10182331,1000),
        (100000000,5000000000,-10000000,9800533,1000)]
