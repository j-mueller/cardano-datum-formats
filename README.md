# cardano-datum-formats

Typed Haskell representations of Cardano protocol datums, plus tests against roundtrips and real mainnet examples.

## Supported Protocols

| Protocol | Supported datum families | Stability |
| --- | --- | --- |
| MinSwap | Orders, pools | 🟢 |
| Sundae | Orders, pools | 🟢 |
| Indigo | `CDP`, `IAsset`, `Gov`, `InterestOracle`, `LRP`, `PriceOracle`, `StabilityPool`, `Staking` | 🟢 |
| Pulse | `MarketInfo`, `Market`, `Order`, `SYVault`, `Oracle`, `YTStake` | 🟢 |
| Strike | `Order`, `Pool`, `Position`, `Settings` | 🟢 |
| Liqwid | `Action`, `Loan`, `Market` | 🟠 |
| WingRiders | Pools | 🟢 |

Protocol-specific modules live under `Cardano.Protocol.*`.

Stability notes:

- `🟢`: derived primarily from protocol-owned contract code, SDKs, or equivalent primary sources, then checked against live chain data.
- `🟠`: tested against live chain data, but some format recovery relied on non-primary public sources. This currently applies to Liqwid, where the public contract source situation was incomplete and we had to lean on third-party code plus live datum inspection.

For notes on how new datum formats are added, see [`docs/adding-datum-formats.md`](./docs/adding-datum-formats.md).

## Build And Test

Builds and tests should be run inside the Nix dev shell.

```sh
nix develop .#ghc966 -c cabal build all
nix develop .#ghc966 -c cabal test cardano-datum-formats-test --test-show-details=direct
```

Do not run project verification commands outside `nix develop`.

## datum-collector

`datum-collector` is a small utility for collecting live inline datums from the chain so they can be turned into unit-test fixtures.

It accepts either:

- a full Cardano address
- a payment credential

Examples:

```sh
nix run .#datum-collector -- addr1...
nix run .#datum-collector -- script:e0302560ced2fdcbfcb2602697df970cd0d6a38f94b32703f51c312b
nix run .#datum-collector -- key:121fd22e...
```

It queries UTxOs at the target, filters to outputs with inline datums, and prints a few distinct datum values as serialized CBOR hex, one per line.

The app will source `./.env` from the current working directory if it exists. A sample environment file is provided in [`.env.example`](./.env.example).

Required environment variable:

```sh
CARDANO_DATUM_FORMATS_BLOCKFROST_PROJECT=...
```

## Project Layout

- `src/cardano-datum-formats/lib`: library modules for datum types and codecs
- `src/cardano-datum-formats/test`: roundtrip and fixture-based tests
- `src/cardano-datum-formats/exe/datum-collector`: live datum collection CLI
- `docs/adding-datum-formats.md`: workflow for adding support for a new protocol or datum family
