{ pkgs, project, ghc, withHoogle ? false }:

let
  inherit (pkgs) lib;

  tools = [
    pkgs.cabal-install
    pkgs.haskellPackages.cabal-fmt
    pkgs.fourmolu
    pkgs.hlint
    pkgs.git
    pkgs.pkg-config
  ];

  nativeLibs = [
    pkgs.blst
    pkgs.bzip2
    pkgs.lmdb
    pkgs.lmdb.dev
    pkgs.libsodium
    pkgs.libsodium.dev
    pkgs.secp256k1
    pkgs.xz
    pkgs.zlib
    pkgs.zstd
  ];

  cardanoPkgConfigAliases = pkgs.runCommand "cardano-pkg-config-aliases" { } ''
    mkdir -p "$out/lib/pkgconfig"
    ln -s ${pkgs.libsodium.dev}/lib/pkgconfig/libsodium.pc "$out/lib/pkgconfig/libsodium-any.pc"
    ln -s ${pkgs.blst}/lib/pkgconfig/libblst.pc "$out/lib/pkgconfig/libblst-any.pc"
    ln -s ${pkgs.secp256k1}/lib/pkgconfig/libsecp256k1.pc "$out/lib/pkgconfig/libsecp256k1-any.pc"
  '';
in
pkgs.mkShell {
  name = "cardano-datum-formats-${ghc}-franken";

  packages = tools ++ nativeLibs ++ [
    cardanoPkgConfigAliases
  ];

  PKG_CONFIG_PATH = lib.makeSearchPath "lib/pkgconfig" (nativeLibs ++ [
    cardanoPkgConfigAliases
  ]);

  LD_LIBRARY_PATH = lib.makeLibraryPath nativeLibs;

  shellHook = ''
    export PS1="\n\[\033[1;32m\][franken:${ghc}:\w]\$\[\033[0m\] "

    if ! command -v ghc-9.10.3 >/dev/null 2>&1; then
      echo "warning: ghc-9.10.3 is not on PATH; cabal.project currently expects it" >&2
    fi
  '';
}
