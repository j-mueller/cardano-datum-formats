{ pkgs, project, ghc, withHoogle ? true }:

let
  tools = {
    cabal = project.projectVariants.${ghc}.tool "cabal" "latest";
    cabal-fmt = project.projectVariants.${ghc}.tool "cabal-fmt" "latest";
    fourmolu = project.projectVariants.${ghc}.tool "fourmolu" "latest";
    hlint = project.projectVariants.${ghc}.tool "hlint" "latest";
  };
in
project.shellFor {
  name = "cardano-datum-formats-${ghc}";
  withHoogle = withHoogle;

  buildInputs = [
    tools.cabal
    tools.cabal-fmt
    tools.fourmolu
    tools.hlint
    pkgs.git
  ];

  shellHook = ''
    export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
  '';
}
