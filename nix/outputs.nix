{ inputs, system }:

let
  pkgs = import ./pkgs.nix { inherit inputs system; };
  lib = pkgs.lib;
  project = import ./project.nix { inherit inputs pkgs lib; };
  projectFlake = project.flake {};

  mkShell = { ghc, withHoogle ? true }:
    import ./shell.nix { inherit pkgs project ghc withHoogle; };
in
{
  devShells = rec {
    default = ghc966;
    ghc966 = mkShell { ghc = "ghc966"; };
    ghc966-nohoogle = mkShell {
      ghc = "ghc966";
      withHoogle = false;
    };
  };

  inherit (projectFlake) apps packages;
  inherit project;
}
