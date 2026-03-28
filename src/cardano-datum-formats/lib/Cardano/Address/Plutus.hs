{-# LANGUAGE TemplateHaskell #-}

module Cardano.Address.Plutus (
    PlutusAddress (..),
    fromAddress,
    toAddress,
    _PlutusPubKeyHashStake,
    _PlutusScriptHash,
    PlutusStakeCredential (..),
) where

import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as L
import Cardano.Data qualified as D
import Cardano.Ledger.Credential qualified as Shelley
import Cardano.Ledger.Plutus.TxInfo qualified as Plutus
import Cardano.Protocol.JSON ()
import Control.Lens ((&), (?~))
import Control.Lens qualified as L
import Convex.CardanoApi.Lenses qualified as L
import Convex.PlutusLedger.V1 qualified as Convex
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.TypeScript.TH (TypeScript (..), deriveTypeScript)
import Data.ByteString.Base16 qualified as Base16
import Data.OpenApi (NamedSchema (..))
import Data.OpenApi.Internal (OpenApiType (OpenApiString))
import Data.OpenApi.Lens qualified as OpenApiL
import Data.OpenApi.Schema qualified as Schema
import Data.OpenApi.SchemaOptions qualified as SchemaOptions
import Data.Proxy (Proxy (..))
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusTx qualified
import PlutusTx.Builtins qualified as PlutusTx
import Test.Gen.Cardano.Api.Typed qualified as Gen
import Test.QuickCheck.Arbitrary (Arbitrary (..))
import Test.QuickCheck.Hedgehog qualified as H

newtype PlutusAddress = PlutusAddress (L.Credential L.Payment, L.StakeReference)
    deriving stock (Eq, Ord, Show)

fromAddress :: C.Address C.ShelleyAddr -> PlutusAddress
fromAddress = \case
    C.ShelleyAddress _ payment stake -> PlutusAddress (payment, stake)

toAddress :: L.Network -> PlutusAddress -> C.Address C.ShelleyAddr
toAddress network (PlutusAddress (payment, stakeRef)) =
    C.ShelleyAddress network payment stakeRef

instance Arbitrary PlutusAddress where
    arbitrary = fromAddress <$> H.hedgehog Gen.genAddressShelley

instance PlutusTx.ToData PlutusAddress where
    toBuiltinData (PlutusAddress (payment, stakeRef)) =
        PlutusTx.toBuiltinData (PV1.Address (Plutus.transCred payment) (Plutus.transStakeReference stakeRef))

instance PlutusTx.FromData PlutusAddress where
    fromBuiltinData dt = do
        PV1.Address{PV1.addressCredential, PV1.addressStakingCredential} <- PlutusTx.fromBuiltinData @PV1.Address dt
        addr <- case addressCredential of
            PV1.PubKeyCredential pkh -> C.PaymentCredentialByKey <$> L.preview L._PlutusPubKeyHash pkh
            PV1.ScriptCredential sh -> C.PaymentCredentialByScript <$> L.preview _PlutusScriptHash sh
        cred <- case addressStakingCredential of
            Nothing -> pure C.NoStakeAddress
            Just (PV1.StakingHash (PV1.PubKeyCredential pkh)) -> C.StakeAddressByValue . C.StakeCredentialByKey <$> L.preview _PlutusPubKeyHashStake pkh
            Just (PV1.StakingHash (PV1.ScriptCredential skh)) -> C.StakeAddressByValue . C.StakeCredentialByScript <$> L.preview _PlutusScriptHash skh
            Just (PV1.StakingPtr _ _ _) -> Nothing
        pure (PlutusAddress (L.view L._PaymentCredential addr, L.view _StakeReference cred))

_PlutusScriptHash :: L.Prism' PV1.ScriptHash C.ScriptHash
_PlutusScriptHash = L.prism' from to
  where
    from :: C.ScriptHash -> PV1.ScriptHash
    from = PV1.ScriptHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

    to :: PV1.ScriptHash -> Maybe C.ScriptHash
    to (PV1.ScriptHash h) =
        either (const Nothing) Just $ C.deserialiseFromRawBytes C.AsScriptHash $ PlutusTx.fromBuiltin h

_StakeReference :: L.Iso' C.StakeAddressReference L.StakeReference
_StakeReference = L.iso from to
  where
    from :: C.StakeAddressReference -> L.StakeReference
    from = \case
        C.NoStakeAddress -> Shelley.StakeRefNull
        C.StakeAddressByValue val -> Shelley.StakeRefBase (C.toShelleyStakeCredential val)
        C.StakeAddressByPointer ptr -> Shelley.StakeRefPtr (C.unStakeAddressPointer ptr)

    to = C.fromShelleyStakeReference

_PlutusPubKeyHashStake :: L.Prism' PV1.PubKeyHash (C.Hash C.StakeKey)
_PlutusPubKeyHashStake = L.prism' from to
  where
    from :: C.Hash C.StakeKey -> PV1.PubKeyHash
    from = PV1.PubKeyHash . PlutusTx.toBuiltin . C.serialiseToRawBytes

    to :: PV1.PubKeyHash -> Maybe (C.Hash C.StakeKey)
    to (PV1.PubKeyHash h) =
        either (const Nothing) Just $
            C.deserialiseFromRawBytes (C.proxyToAsType $ Proxy @(C.Hash C.StakeKey)) $
                PlutusTx.fromBuiltin h

newtype PlutusStakeCredential = PlutusStakeCredential
    { getPlutusStakeCredential :: C.StakeCredential
    }
    deriving stock (Eq, Show, Generic)

instance Arbitrary PlutusStakeCredential where
    arbitrary = PlutusStakeCredential <$> H.hedgehog Gen.genStakeCredential

instance PlutusTx.ToData PlutusStakeCredential where
    toBuiltinData (PlutusStakeCredential cred) =
        PlutusTx.mkConstr 0 [PlutusTx.toBuiltinData (Convex.transStakeCredential cred)]

instance PlutusTx.FromData PlutusStakeCredential where
    fromBuiltinData cred = D.withConstr cred $ \case
        (0, [x]) -> do
            value <- PlutusTx.fromBuiltinData x
            either (const Nothing) (Just . PlutusStakeCredential) (Convex.unTransStakeCredential value)
        _ -> Nothing

instance ToJSON PlutusAddress where
    toJSON =
        Aeson.String
            . TE.decodeUtf8
            . Base16.encode
            . C.serialiseToCBOR
            . C.fromPlutusData
            . PlutusTx.toData

instance FromJSON PlutusAddress where
    parseJSON =
        Aeson.withText "PlutusAddress" $ \text -> do
            rawBytes <- either (fail . show) pure $ Base16.decode (TE.encodeUtf8 text)
            scriptData <- either (fail . show) pure $ C.deserialiseFromCBOR C.AsScriptData rawBytes
            maybe (fail "failed to decode PlutusAddress") pure $ PlutusTx.fromData (C.toPlutusData scriptData)

instance Schema.ToSchema PlutusAddress where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "PlutusAddress") $
                mempty
                    & OpenApiL.type_ ?~ OpenApiString
                    & OpenApiL.description ?~ "hex-encoded CBOR serialised Plutus address"

instance TypeScript PlutusAddress where
    getTypeScriptType _ = "string"

instance ToJSON PlutusStakeCredential where
    toJSON =
        Aeson.String
            . TE.decodeUtf8
            . Base16.encode
            . C.serialiseToCBOR
            . C.fromPlutusData
            . PlutusTx.toData

    toEncoding =
        Aeson.toEncoding
            . TE.decodeUtf8
            . Base16.encode
            . C.serialiseToCBOR
            . C.fromPlutusData
            . PlutusTx.toData

instance FromJSON PlutusStakeCredential where
    parseJSON =
        Aeson.withText "PlutusStakeCredential" $ \text -> do
            rawBytes <- either (fail . show) pure $ Base16.decode (TE.encodeUtf8 text)
            scriptData <- either (fail . show) pure $ C.deserialiseFromCBOR C.AsScriptData rawBytes
            maybe (fail "failed to decode PlutusStakeCredential") pure $ PlutusTx.fromData (C.toPlutusData scriptData)

instance TypeScript PlutusStakeCredential where
    getTypeScriptType _ = "string"

instance Schema.ToSchema PlutusStakeCredential where
    declareNamedSchema _ =
        pure $
            NamedSchema (Just "PlutusStakeCredential") $
                mempty
                    & OpenApiL.type_ ?~ OpenApiString
                    & OpenApiL.description ?~ "hex-encoded CBOR serialised Plutus stake credential"
