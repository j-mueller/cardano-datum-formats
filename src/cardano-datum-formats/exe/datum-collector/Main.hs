module Main (main) where

import Blockfrost.Auth (mkProject)
import Blockfrost.Client.Auth (Project)
import Cardano.Api qualified as C
import Control.Lens qualified as L
import Control.Monad.Except (ExceptT (..), runExceptT)
import Convex.Blockfrost (BlockfrostT, evalBlockfrostT)
import Convex.CardanoApi.Lenses qualified as L
import Convex.Class qualified as Chain
import Convex.Utxos qualified as Utxos
import Data.Bifunctor (first)
import Data.ByteString.Base16 qualified as Base16
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Options.Applicative (
    Parser,
    ParserInfo,
    argument,
    customExecParser,
    disambiguate,
    fullDesc,
    header,
    help,
    helper,
    info,
    metavar,
    prefs,
    progDesc,
    showHelpOnEmpty,
    showHelpOnError,
    str,
 )
import System.Environment (lookupEnv)
import System.Exit (die)

newtype Args = Args
    { target :: String
    }

type AppM = ExceptT String (BlockfrostT IO)

main :: IO ()
main = do
    Args{target} <- customExecParser opts prefsInfo
    project <- getBlockfrostProject
    paymentCredential <- either die pure (parseTarget target)
    datums <- runCollector project paymentCredential
    case datums of
        [] -> die "No inline datums found for this target"
        xs -> mapM_ (putStrLn . Text.unpack) xs
  where
    opts = prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError

prefsInfo :: ParserInfo Args
prefsInfo =
    info
        (helper <*> parseArgs)
        ( fullDesc
            <> progDesc "Collect a few distinct inline datum values for an address or payment credential"
            <> header "datum-collector"
        )

parseArgs :: Parser Args
parseArgs =
    Args
        <$> argument
            str
            (metavar "TARGET" <> help "A Shelley address, or a payment credential as key:<hex>, script:<hex>, or bare key-hash hex")

getBlockfrostProject :: IO Project
getBlockfrostProject =
    lookupEnv "CARDANO_DATUM_FORMATS_BLOCKFROST_PROJECT" >>= \case
        Just project | not (null project) -> pure (mkProject $ Text.pack project)
        _ -> die "Missing CARDANO_DATUM_FORMATS_BLOCKFROST_PROJECT"

parseTarget :: String -> Either String C.PaymentCredential
parseTarget raw =
    maybe
        (parsePaymentCredential raw)
        paymentCredentialFromAddress
        (C.deserialiseAddress C.AsAddressAny (Text.pack raw))

paymentCredentialFromAddress :: C.AddressAny -> Either String C.PaymentCredential
paymentCredentialFromAddress = \case
    C.AddressByron{} ->
        Left "Byron addresses are not supported; use a Shelley address or payment credential"
    C.AddressShelley address ->
        case address of
            C.ShelleyAddress _ paymentCredential _ ->
                Right (C.fromShelleyPaymentCredential paymentCredential)

parsePaymentCredential :: String -> Either String C.PaymentCredential
parsePaymentCredential input =
    case splitPrefix input of
        ("key", hex) ->
            C.PaymentCredentialByKey <$> parseRawBytes (C.proxyToAsType (Proxy @(C.Hash C.PaymentKey))) hex
        ("script", hex) ->
            C.PaymentCredentialByScript <$> parseRawBytes C.AsScriptHash hex
        ("", hex) ->
            C.PaymentCredentialByKey <$> parseRawBytes (C.proxyToAsType (Proxy @(C.Hash C.PaymentKey))) hex
        (prefix, _) ->
            Left $ "Unsupported target prefix: " <> prefix
  where
    splitPrefix value = case break (== ':') value of
        (prefix, ':' : rest) -> (prefix, rest)
        _ -> ("", value)

parseRawBytes :: C.SerialiseAsRawBytes a => C.AsType a -> String -> Either String a
parseRawBytes asType hex =
    first show $
        C.deserialiseFromRawBytes
            asType
            (either error id $ Base16.decode $ Text.encodeUtf8 $ Text.pack hex)

runCollector :: Project -> C.PaymentCredential -> IO [Text]
runCollector project paymentCredential = do
    result <- evalBlockfrostT project (runExceptT collect)
    case result of
        Left err -> die (show err)
        Right (Left err) -> die err
        Right (Right values) -> pure values
  where
    collect :: AppM [Text]
    collect = do
        C.UTxO utxos <- Utxos.toApiUtxo @C.ConwayEra <$> Chain.utxosByPaymentCredential paymentCredential
        let datumHexes =
                take 5
                    . nub
                    . mapMaybe (txOutInlineDatumHex . snd)
                    $ Map.toList utxos
        pure datumHexes

txOutInlineDatumHex :: C.IsBabbageBasedEra era => C.TxOut C.CtxUTxO era -> Maybe Text
txOutInlineDatumHex txOut = do
    datum <- L.preview (L._TxOut . L._3 . L._TxOutDatumInline) txOut
    pure $
        Text.decodeUtf8 $
            Base16.encode $
                C.serialiseToCBOR $
                    C.getScriptData datum
