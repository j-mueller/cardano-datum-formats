{
  description = "cardano-datum-formats";

  inputs = {
    haskell-nix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.hackage.follows = "hackage";
    };

    # nixpkgs 24.11 provides the GHC 9.4.5 bootstrap compiler used by haskell.nix.
    nixpkgs.follows = "haskell-nix/nixpkgs-2411";
    nixpkgs-unstable.follows = "haskell-nix/nixpkgs-unstable";

    hackage = {
      url = "github:input-output-hk/hackage.nix";
      flake = false;
    };

    CHaP = {
      url = "github:IntersectMBO/cardano-haskell-packages?ref=repo";
      flake = false;
    };

    iohk-nix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    systems.url = "github:nix-systems/default";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      import ./nix/outputs.nix { inherit inputs system; }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
