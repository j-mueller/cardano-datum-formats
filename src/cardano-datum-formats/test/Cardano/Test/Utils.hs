module Cardano.Test.Utils (
    datumRoundTrip,
    fromHexRawBytes,
    hexToBuiltinByteString,
    scriptDataFromHex,
) where

import Cardano.Api qualified as C
import Cardano.Data qualified as Datum
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Enc
import PlutusTx.Builtins qualified as PlutusTx
import PlutusTx qualified

datumRoundTrip :: forall a. (PlutusTx.ToData a, PlutusTx.FromData a, Eq a) => a -> Bool
datumRoundTrip value = case PlutusTx.fromData (PlutusTx.toData value) of
    Just decoded -> decoded == value
    Nothing -> False

fromHexRawBytes :: forall a. C.SerialiseAsRawBytes a => C.AsType a -> String -> a
fromHexRawBytes asType hex =
    either (error . show) id $
        C.deserialiseFromRawBytes asType (either error id $ Base16.decode $ Enc.encodeUtf8 $ Text.pack hex)

hexToBuiltinByteString :: String -> PlutusTx.BuiltinByteString
hexToBuiltinByteString hex =
    PlutusTx.toBuiltin (either error id $ Base16.decode $ Enc.encodeUtf8 $ Text.pack hex)

scriptDataFromHex :: String -> Either String C.ScriptData
scriptDataFromHex = Datum.fromCborHex @C.ScriptData
