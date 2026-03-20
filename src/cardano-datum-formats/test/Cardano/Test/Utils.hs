module Cardano.Test.Utils (
    datumRoundTrip,
) where

import PlutusTx qualified

datumRoundTrip :: forall a. (PlutusTx.ToData a, PlutusTx.FromData a, Eq a) => a -> Bool
datumRoundTrip value = case PlutusTx.fromData (PlutusTx.toData value) of
    Just decoded -> decoded == value
    Nothing -> False
