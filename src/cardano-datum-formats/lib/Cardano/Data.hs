module Cardano.Data (
    withConstr,
    getB,
    getI,
    deserialiseHash,
    serialiseHash,
    deserialiseScriptHash,
    serialiseScriptHash,
    fromBuiltinDataHex,
    fromCborHex,
) where

import Cardano.Api qualified as C
import Data.ByteString.Base16 qualified as Base16
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Enc
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx

withConstr :: PlutusTx.BuiltinData -> ((Integer, [PlutusTx.BuiltinData]) -> Maybe r) -> Maybe r
withConstr dt match =
    PlutusTx.matchData
        dt
        (curry match)
        (const Nothing)
        (const Nothing)
        (const Nothing)
        (const Nothing)

getB :: PlutusTx.BuiltinData -> Maybe PlutusTx.BuiltinByteString
getB dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        (const Nothing)
        (const Nothing)
        Just

getI :: PlutusTx.BuiltinData -> Maybe Integer
getI dt =
    PlutusTx.matchData
        dt
        (\_ _ -> Nothing)
        (const Nothing)
        (const Nothing)
        Just
        (const Nothing)

deserialiseHash :: forall h. C.SerialiseAsRawBytes (C.Hash h) => PlutusTx.BuiltinByteString -> Maybe (C.Hash h)
deserialiseHash bs =
    case C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash h)) (PlutusTx.fromBuiltin bs) of
        Left{} -> Nothing
        Right k -> Just k

serialiseHash :: C.SerialiseAsRawBytes (C.Hash h) => C.Hash h -> PlutusTx.BuiltinData
serialiseHash = PlutusTx.mkB . PlutusTx.toBuiltin . C.serialiseToRawBytes

deserialiseScriptHash :: PlutusTx.BuiltinByteString -> Maybe C.ScriptHash
deserialiseScriptHash bs =
    case C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @C.ScriptHash) (PlutusTx.fromBuiltin bs) of
        Left{} -> Nothing
        Right k -> Just k

serialiseScriptHash :: C.ScriptHash -> PlutusTx.BuiltinData
serialiseScriptHash = PlutusTx.mkB . PlutusTx.toBuiltin . C.serialiseToRawBytes

fromBuiltinDataHex :: forall a. PlutusTx.FromData a => String -> Either String a
fromBuiltinDataHex hex = do
    datum <- fromCborHex hex
    let value = PlutusTx.fromBuiltinData @a $ PlutusTx.toBuiltinData $ PlutusTx.dataToBuiltinData $ C.toPlutusData datum
    maybe (Left "fromBuiltinDataHex: PlutusTx.fromBuiltinData failed") Right value

fromCborHex :: forall a. C.SerialiseAsCBOR a => String -> Either String a
fromCborHex hex =
    either
        (Left . show)
        pure
        (C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @a) (either error id $ Base16.decode $ Enc.encodeUtf8 $ Text.pack hex))
