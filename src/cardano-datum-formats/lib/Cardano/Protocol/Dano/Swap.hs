module Cardano.Protocol.Dano.Swap (SwapEntry (..), SwapRedeemer (..), encodeSwapRedeemer, decodeSwapRedeemer) where

import Control.Monad (guard)
import Data.ByteString qualified as BS
import PlutusTx.Builtins qualified as B
import Cardano.Data qualified as D

-- Indices refer to the final sorted transaction inputs/reference inputs.
data SwapEntry = SwapEntry { poolInputIndex :: Integer, poolOutputIndex :: Integer, deltaAmount :: Integer }
    deriving stock (Eq, Show)
data SwapRedeemer = SwapRedeemer { contextIndex :: Integer, swapEntries :: [SwapEntry] }
    deriving stock (Eq, Show)

encodeSwapRedeemer :: SwapRedeemer -> Maybe B.BuiltinData
encodeSwapRedeemer (SwapRedeemer index entries) = do
    guard (byteIndex index && not (null entries))
    encoded <- traverse encodeEntry entries
    pure . B.mkB . B.toBuiltin $ BS.pack [fromInteger index,3] <> BS.concat encoded
  where
    encodeEntry (SwapEntry input output delta) = do
        guard (byteIndex input && byteIndex output && delta /= 0 && delta >= negate (2^ (255 :: Int)) && delta < 2^ (255 :: Int))
        let unsigned = delta `mod` (2^ (256 :: Int))
        pure $ BS.pack ([fromInteger input,fromInteger output] <> [fromInteger ((unsigned `div` (256^i)) `mod` 256) | i <- [31,30..0 :: Int]])

decodeSwapRedeemer :: B.BuiltinData -> Maybe SwapRedeemer
decodeSwapRedeemer datum = do
    bytes <- B.fromBuiltin <$> D.getB datum
    guard (BS.length bytes >= 36 && (BS.length bytes-2) `mod` 34 == 0 && BS.index bytes 1 == 3)
    entries <- decodeEntries (BS.drop 2 bytes)
    pure $ SwapRedeemer (toInteger $ BS.head bytes) entries
  where
    decodeEntries bytes
        | BS.null bytes = Just []
        | otherwise = do
            let unsigned = foldl' (\n b -> n*256+toInteger b) 0 (BS.unpack $ BS.take 32 $ BS.drop 2 bytes)
                delta = if unsigned >= 2^ (255 :: Int) then unsigned-2^ (256 :: Int) else unsigned
            guard (delta /= 0)
            rest <- decodeEntries (BS.drop 34 bytes)
            pure $ SwapEntry (toInteger $ BS.head bytes) (toInteger $ BS.index bytes 1) delta : rest

byteIndex :: Integer -> Bool
byteIndex n = n >= 0 && n <= 255
