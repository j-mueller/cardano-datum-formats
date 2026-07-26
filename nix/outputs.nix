{ inputs, system }:

let
  pkgs = import ./pkgs.nix { inherit inputs system; };
  lib = pkgs.lib;
  project = import ./project.nix { inherit inputs pkgs lib; };
  projectFlake = project.flake {};
  datumCollectorApp = pkgs.writeShellScriptBin "datum-collector" ''
    if [ -f .env ]; then
      # shellcheck disable=SC1091
      set -a
      . ./.env
      set +a
    fi

    exec ${lib.getExe projectFlake.packages."cardano-datum-formats:exe:datum-collector"} "$@"
  '';
  apps = projectFlake.apps // {
    datum-collector = {
      type = "app";
      program = lib.getExe datumCollectorApp;
    };
  };

  mkShell = { ghc, withHoogle ? true }:
    import ./shell.nix { inherit pkgs project ghc withHoogle; };
in
{
  devShells = rec {
    default = ghc9103;
    ghc9103 = mkShell {
      ghc = "ghc9103";
      withHoogle = false;
    };
    ghc9103-nohoogle = mkShell {
      ghc = "ghc9103";
      withHoogle = false;
    };
  };

  inherit apps;
  inherit (projectFlake) packages;
  inherit project;
}
