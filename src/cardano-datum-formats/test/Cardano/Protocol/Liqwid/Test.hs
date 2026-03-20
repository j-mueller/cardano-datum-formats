module Cardano.Protocol.Liqwid.Test (
    tests,
) where

import Cardano.Data qualified as Datum
import Cardano.Protocol.Liqwid.Action qualified as Action
import Cardano.Protocol.Liqwid.Common qualified as Common
import Cardano.Protocol.Liqwid.Loan qualified as Loan
import Cardano.Protocol.Liqwid.Market qualified as Market
import Cardano.Test.Utils (datumRoundTrip, hexToBuiltinByteString)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "liqwid"
        [ testGroup
            "common"
            [ testProperty "PubKeyHash" (datumRoundTrip @Common.PubKeyHash)
            , testProperty "BatchState" (datumRoundTrip @Common.BatchState)
            , testProperty "IntegerPair" (datumRoundTrip @Common.IntegerPair)
            ]
        , testGroup
            "datum roundtrips"
            [ testProperty "ActionDatum" (datumRoundTrip @Action.ActionDatum)
            , testProperty "LoanDatum" (datumRoundTrip @Loan.LoanDatum)
            , testProperty "MarketDatum" (datumRoundTrip @Market.MarketDatum)
            ]
        , testGroup
            "mainnet decode"
            [ HUnit.testCase "decode action datum from live action script" decodeActionDatum
            , HUnit.testCase "decode historical action datum with negative state" decodeNegativeActionDatum
            , HUnit.testCase "decode loan datum" decodeLoanDatum
            , HUnit.testCase "decode market datum" decodeMarketDatum
            ]
        ]

decodeActionDatum :: HUnit.Assertion
decodeActionDatum = do
    let actionHex = "9f9f0000000000ff1a1de717a7ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Action.ActionDatum actionHex)
    HUnit.assertEqual "batch state" (Common.BatchState 0 0 0 0 0) (Action.adBatchState datum)
    HUnit.assertEqual "reserved supply" 501684135 (Action.adReservedSupply datum)

decodeNegativeActionDatum :: HUnit.Assertion
decodeNegativeActionDatum = do
    let actionHex = "9f9f00003a05e403c63a0011dd3800ff1a08b03ecaff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Action.ActionDatum actionHex)
    HUnit.assertEqual "batch state" (Common.BatchState 0 0 (-98829255) (-1170745) 0) (Action.adBatchState datum)
    HUnit.assertEqual "reserved supply" 145768138 (Action.adReservedSupply datum)

decodeLoanDatum :: HUnit.Assertion
decodeLoanDatum = do
    let loanHex = "9f581cbd628baabcb0223c8b91831a75287cd1aaccc3b60b428db510ccd1f31a0bebc200001a000271001b003ba4fd148c24eeff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Loan.LoanDatum loanHex)
    HUnit.assertEqual "borrower" (Common.PubKeyHash $ hexToBuiltinByteString "bd628baabcb0223c8b91831a75287cd1aaccc3b60b428db510ccd1f3") (Loan.ldBorrower datum)
    HUnit.assertEqual "principal" 200000000 (Loan.ldPrincipal datum)
    HUnit.assertEqual "accrued interest" 0 (Loan.ldAccruedInterest datum)
    HUnit.assertEqual "minimum interest" 160000 (Loan.ldMinInterest datum)
    HUnit.assertEqual "interest index" 16788430504338670 (Loan.ldInterestIndex datum)

decodeMarketDatum :: HUnit.Assertion
decodeMarketDatum = do
    let marketHex = "9f1b000012006ac282b11a8b03eca01b000603091d115eef1b00000cd027e4f6fd1b00000018a509693c1b003c35573bb7abc99fc24a4d6c76163a13a9c7de4fc24ca4dbd3af64e0000000000000ff1b00000186b636f3601b00000186b64658c89f1b00001ee30e6e889b1b000603091d115eefff1a002dc6c0ff"
    datum <- either HUnit.assertFailure pure (Datum.fromBuiltinDataHex @Market.MarketDatum marketHex)
    HUnit.assertEqual "supply" 19793000432305 (Market.mdSupply datum)
    HUnit.assertEqual "reserve" 2332290208 (Market.mdReserve datum)
    HUnit.assertEqual "qTokens" 1692187537530607 (Market.mdQTokens datum)
    HUnit.assertEqual "principal" 14088162047741 (Market.mdPrincipal datum)
    HUnit.assertEqual "interest" 105848072508 (Market.mdInterest datum)
    HUnit.assertEqual "interest index" 16947147382959049 (Market.mdInterestIndex datum)
    HUnit.assertEqual "interest rate" (Common.IntegerPair 365622976593325403397711 51021296019973103253012348928) (Market.mdInterestRate datum)
    HUnit.assertEqual "last interest time" 1678094300000 (Market.mdLastInterestTime datum)
    HUnit.assertEqual "last batch" 1678095309000 (Market.mdLastBatch datum)
    HUnit.assertEqual "q token rate" (Common.IntegerPair 33960548534427 1692187537530607) (Market.mdQTokenRate datum)
    HUnit.assertEqual "min ada" 3000000 (Market.mdMinAda datum)
