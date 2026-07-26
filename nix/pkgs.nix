{ inputs, system }:

let
  upstreamPkgs = import inputs.nixpkgs-unstable { inherit system; };
in
import inputs.nixpkgs {
  inherit system;
  config = inputs.haskell-nix.config;
  overlays = [
    inputs.iohk-nix.overlays.crypto
    inputs.iohk-nix.overlays.cardano-lib
    inputs.haskell-nix.overlay
    inputs.iohk-nix.overlays.haskell-nix-crypto
    inputs.iohk-nix.overlays.haskell-nix-extra
    # cardano-crypto-class >= 2.3.2 requires BLST >= 0.3.14.
    (_final: _prev: { inherit (upstreamPkgs) blst; })
  ];
}
