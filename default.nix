{ pkgs ? (import <nixpkgs> {}), hs ? pkgs.haskellPackages, ... }:
let
  inherit (hs) cabal hdevtools time text hashable SHA bytestring;
  cabalInstall = hs.cabalInstall_1_18_0_3;
in cabal.mkDerivation (self: {
  pname = "hiak";
  version = "0.1.0.0";
  src = ./.;

  buildDepends = [ time text hashable bytestring SHA ];
  extraLibraries = [ hdevtools ];
  buildTools = [ cabalInstall ];

  enableSplitObjs = false;
})
