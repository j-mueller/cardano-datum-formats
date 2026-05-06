{ inputs, pkgs, lib }:

pkgs.haskell-nix.cabalProject' ({ ... }: {
  name = "cardano-datum-formats";
  compiler-nix-name = lib.mkDefault "ghc9103";
  src = lib.cleanSource ../.;

  flake.variants = {
    ghc9103 = {};
  };

  inputMap = {
    "https://chap.intersectmbo.org/" = inputs.CHaP;
  };

  cabalProjectLocal = ''
    package *
      ghc-options=-Werror
  '';
})
