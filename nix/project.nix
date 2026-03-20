{ inputs, pkgs, lib }:

pkgs.haskell-nix.cabalProject' ({ ... }: {
  name = "cardano-datum-formats";
  compiler-nix-name = lib.mkDefault "ghc966";
  src = lib.cleanSource ../.;

  flake.variants = {
    ghc966 = {};
  };

  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
  };

  cabalProjectLocal = ''
    package *
      ghc-options=-Werror
  '';
})
