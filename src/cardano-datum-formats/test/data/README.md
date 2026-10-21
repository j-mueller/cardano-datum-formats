# Dano validator fixture

`dano-mainnet-validator.cbor` contains the public Plutus V3 reference script at mainnet output `64d111b957e7d7848ffdde5149aa77fa4090a7fa1ad0ac108067900614848501#0`.

Its script hash is `d8b69fc53637bcfadbc4469083f706bc293f4d9d2296646c5ca167bb`; the emulator checks this before use. Tests seed authenticated pool/configuration UTxOs and exercise both native-token swap directions through balancing and ledger submission. An excess-output case must fail validation.

Datum layouts, quote rounding and packed redeemers follow the official `dano-finance/clmm-sdk` commit `0416dc1f3531ce77eec7664ea3947f192a3aca40`. `Cardano.Protocol.Dano.Transaction` supports native-token pools. ADA pools need staking-reward handling beyond this builder.
