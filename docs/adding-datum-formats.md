# Adding Datum Formats

This repo is for collecting typed representations of on-chain datum formats, plus test vectors that prove we can decode real chain data.

The general workflow is:

1. Find a primary source for the datum shape.
2. Implement a typed Haskell representation.
3. Add decode tests from SDK or contract vectors.
4. Add decode tests from real mainnet datums.
5. Build and test in the Nix dev shell.

## Primary Sources

Prefer protocol-owned sources over third-party descriptions:

- protocol SDKs
- contract type definitions
- on-chain contract repositories
- official API responses that expose script hashes or reference inputs

For Sundae, the TypeScript SDK on GitHub was the best source for the current pool datum layout. The key files were the contract type definitions and datum-builder tests.

Use current sources when possible. Older contract JSONs may describe earlier layouts that no longer match what is deployed.

## Module Layout

Put protocol-specific implementations under the protocol namespace.

If a protocol has multiple layouts, eras, or families of datums, prefer a more specific sub-namespace for the implementation module. For example:

- `Cardano.Protocol.<Protocol>.<Family>.Order`
- `Cardano.Protocol.<Protocol>.<Family>.Pool`

If a less-specific top-level module is useful for compatibility, make it a thin reexport of the more specific implementation module.

Keep shared reusable types outside a protocol namespace when they are likely to be reused by other formats.

## Modeling The Datum

When translating an external schema into Haskell:

- match the on-chain constructor and field ordering exactly
- keep constructors and field names close to the source material
- model optional fields as `Maybe`
- represent raw byte identifiers as `BuiltinByteString` when that is what the chain format uses
- be careful with asset encoding, especially ADA vs native assets
- avoid adding interpretation logic until raw decoding is proven correct

The first goal is faithful decoding, not a polished domain model.

## Testing Strategy

Each new datum format should usually get three kinds of tests.

### Roundtrip Tests

Add `ToData`/`FromData` roundtrip properties for the new types.

These catch obvious encoding mismatches quickly, but they are not enough on their own because both directions can be wrong in the same way.

### SDK Or Contract Vectors

If the protocol SDK or contract repo includes expected CBOR hex, copy a few representative examples into unit tests.

These tests should decode the exact hex and assert specific fields, not just that decoding succeeds.

This is the fastest way to prove the implementation matches the protocol’s published format.

### Real Mainnet Fixtures

Use `datum-collector` to capture real inline datums from live addresses or payment credentials and add a small number of decode tests for them.

These tests protect against:

- stale assumptions from old contract sources
- differences between test vectors and deployed chain data
- mistakes in address or asset interpretation

For Sundae pools, we used live pool payment script credentials and collected distinct inline datum CBOR hex values from mainnet.

## Finding Live Datums

The process we used for Sundae is the template for future protocols:

1. Use the protocol SDK or public API to find the relevant script hashes or reference inputs.
2. Determine the payment credential for the script that holds the target UTxOs.
3. Run `datum-collector` with either the full address or the payment credential.
4. Keep a few distinct examples that cover materially different shapes.

When the protocol exposes multiple pool families, collect examples from each family separately.

## Mainnet Address Discovery

If a protocol publishes script hashes but not addresses directly:

1. look for protocol parameter endpoints, reference inputs, or settings UTxOs
2. inspect any settings datum needed to derive stake credentials or script addresses
3. reconstruct the live address only after you understand how the payment and staking credentials are composed

Do not guess addresses from partial information if you can derive them from protocol-owned data.

## Verification

Always verify from inside the Nix dev shell:

```sh
nix develop .#ghc966 -c cabal build all
nix develop .#ghc966 -c cabal test cardano-datum-formats-test --test-show-details=direct
```

If you need live datum examples:

```sh
nix run .#datum-collector -- <address-or-payment-credential>
```

The `datum-collector` app will source `./.env` if present. For Blockfrost-backed collection, set:

```sh
CARDANO_DATUM_FORMATS_BLOCKFROST_PROJECT=...
```

## Practical Notes

- Prefer current SDK code over historical artifacts.
- Keep real fixtures small and targeted.
- Record the source of a fixture in the test comment or adjacent doc if it is not obvious.
- If build or test commands race in parallel, rerun them sequentially before concluding there is a code problem.
- If a `nix run` app cannot see a newly added source file, make sure the file is tracked by git so the flake source includes it.
