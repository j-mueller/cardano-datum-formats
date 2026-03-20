# AGENTS

## Build And Test

Always run builds and tests from inside the Nix dev shell.

Use:

```sh
nix develop .#ghc966 -c cabal build all
nix develop .#ghc966 -c cabal test cardano-datum-formats-test --test-show-details=direct
```

Do not run `cabal build`, `cabal test`, or similar project verification commands outside `nix develop`.
