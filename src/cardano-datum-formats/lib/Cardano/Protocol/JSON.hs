{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Protocol.JSON (
    jsonOptions,
    sumOptions,
    sumOptionsWithFieldModifier,
    stripFieldPrefix,
    stripFieldPrefixes,
) where

import Cardano.Api qualified as C
import Control.Lens ((&), (?~))
import Control.Applicative ((<|>))
import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Aeson.TypeScript.TH (TypeScript (..))
import Data.ByteString.Base16 qualified as Base16
import Data.List (stripPrefix)
import Data.OpenApi (NamedSchema (..))
import Data.OpenApi.Internal (OpenApiType (OpenApiInteger, OpenApiString))
import Data.OpenApi.Lens qualified as L
import Data.OpenApi.Schema (ToSchema (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Typeable (Typeable)
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx

jsonOptions :: Int -> Aeson.Options
jsonOptions prefixLength =
    Aeson.defaultOptions
        { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop prefixLength
        }

sumOptions :: Int -> Aeson.Options
sumOptions prefixLength =
    Aeson.defaultOptions
        { Aeson.constructorTagModifier = Aeson.camelTo2 '_' . drop prefixLength
        , Aeson.sumEncoding = Aeson.ObjectWithSingleField
        }

sumOptionsWithFieldModifier :: Int -> (String -> String) -> Aeson.Options
sumOptionsWithFieldModifier prefixLength fieldModifier =
    (sumOptions prefixLength)
        { Aeson.fieldLabelModifier = fieldModifier
        }

stripFieldPrefix :: String -> String -> String
stripFieldPrefix prefix fieldName =
    Aeson.camelTo2 '_' $
        maybe fieldName id (stripPrefix prefix fieldName)

stripFieldPrefixes :: [String] -> String -> String
stripFieldPrefixes prefixes fieldName =
    Aeson.camelTo2 '_' $
        maybe fieldName id $
            foldr (\prefix acc -> acc <|> stripPrefix prefix fieldName) Nothing prefixes

instance ToJSON C.AssetId where
    toJSON = \case
        C.AdaAssetId -> Aeson.String "lovelace"
        C.AssetId policy assetName ->
            Aeson.String (C.serialiseToRawBytesHexText policy <> "." <> C.serialiseToRawBytesHexText assetName)

instance FromJSON C.AssetId where
    parseJSON =
        Aeson.withText "AssetId" $ \text ->
            case text of
                "lovelace" -> pure C.AdaAssetId
                _ ->
                    case T.breakOn "." text of
                        (policyText, assetText)
                            | T.null assetText -> fail "expected asset id in the form POLICY_ID.TOKEN_NAME"
                            | otherwise ->
                                C.AssetId
                                    <$> either (fail . show) pure (C.deserialiseFromRawBytesHex (TE.encodeUtf8 policyText))
                                    <*> either (fail . show) pure (C.deserialiseFromRawBytesHex (TE.encodeUtf8 (T.drop 1 assetText)))

instance TypeScript C.AssetId where
    getTypeScriptType _ = "string"

instance ToSchema C.AssetId where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "AssetId") $
                mempty
                    & L.type_ ?~ OpenApiString
                    & L.description ?~ "Cardano asset ID, either 'lovelace' or POLICY_ID.TOKEN_NAME"

instance TypeScript C.Quantity where
    getTypeScriptType _ = "number"

instance ToSchema C.Quantity where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "Quantity") $
                mempty
                    & L.type_ ?~ OpenApiInteger

instance (Typeable keyrole, C.SerialiseAsRawBytes (C.Hash keyrole)) => TypeScript (C.Hash keyrole) where
    getTypeScriptType _ = "string"

instance (Typeable keyrole, C.SerialiseAsRawBytes (C.Hash keyrole)) => ToSchema (C.Hash keyrole) where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "Hash") $
                mempty
                    & L.type_ ?~ OpenApiString
                    & L.description ?~ "hex-encoded Cardano hash"

instance TypeScript C.ScriptHash where
    getTypeScriptType _ = "string"

instance ToSchema C.ScriptHash where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ScriptHash") $
                mempty
                    & L.type_ ?~ OpenApiString
                    & L.description ?~ "hex-encoded Cardano script hash"

instance TypeScript C.StakeCredential where
    getTypeScriptType _ = "any"

instance FromJSON C.StakeCredential where
    parseJSON =
        Aeson.withObject "StakeCredential" $ \obj ->
            (C.StakeCredentialByKey <$> (obj Aeson..: "stakingKeyHash" >>= parseStakeKeyHash))
                <|> (C.StakeCredentialByScript <$> obj Aeson..: "stakingScriptHash")

parseStakeKeyHash :: T.Text -> Aeson.Parser (C.Hash C.StakeKey)
parseStakeKeyHash =
    either (fail . show) pure . C.deserialiseFromRawBytesHex . TE.encodeUtf8

instance ToSchema C.StakeCredential where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "StakeCredential") mempty

instance TypeScript (C.Address C.ShelleyAddr) where
    getTypeScriptType _ = "string"

instance ToSchema (C.Address C.ShelleyAddr) where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "ShelleyAddress") $
                mempty
                    & L.type_ ?~ OpenApiString
                    & L.description ?~ "bech32-encoded Shelley address"

instance ToJSON PlutusTx.BuiltinByteString where
    toJSON = Aeson.String . TE.decodeUtf8 . Base16.encode . PlutusTx.fromBuiltin

instance ToJSONKey PlutusTx.BuiltinByteString where
    toJSONKey = Aeson.toJSONKeyText (TE.decodeUtf8 . Base16.encode . PlutusTx.fromBuiltin)

instance FromJSON PlutusTx.BuiltinByteString where
    parseJSON =
        Aeson.withText "BuiltinByteString" $ \text ->
            case Base16.decode (TE.encodeUtf8 text) of
                Left err -> fail err
                Right bytes -> pure (PlutusTx.toBuiltin bytes)

instance FromJSONKey PlutusTx.BuiltinByteString where
    fromJSONKey = Aeson.FromJSONKeyTextParser $ \text ->
        case Base16.decode (TE.encodeUtf8 text) of
            Left err -> fail err
            Right bytes -> pure (PlutusTx.toBuiltin bytes)

instance TypeScript PlutusTx.BuiltinByteString where
    getTypeScriptType _ = "string"

instance ToSchema PlutusTx.BuiltinByteString where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "BuiltinByteString") $
                mempty
                    & L.type_ ?~ OpenApiString
                    & L.description ?~ "hex-encoded bytes"

instance ToJSON PlutusTx.BuiltinData where
    toJSON =
        Aeson.String
            . TE.decodeUtf8
            . Base16.encode
            . C.serialiseToCBOR
            . C.fromPlutusData
            . PlutusTx.builtinDataToData

instance FromJSON PlutusTx.BuiltinData where
    parseJSON =
        Aeson.withText "BuiltinData" $ \text -> do
            rawBytes <- either (fail . show) pure $ Base16.decode (TE.encodeUtf8 text)
            scriptData <- either (fail . show) pure $ C.deserialiseFromCBOR C.AsScriptData rawBytes
            pure $ PlutusTx.dataToBuiltinData (C.toPlutusData scriptData)

instance TypeScript PlutusTx.BuiltinData where
    getTypeScriptType _ = "string"

instance ToSchema PlutusTx.BuiltinData where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "BuiltinData") $
                mempty
                    & L.type_ ?~ OpenApiString
                    & L.description ?~ "hex-encoded CBOR serialised Plutus data"
