{ pkgs ? (import <nixpkgs> {}), ... }:
let
  haskellPackages = pkgs.haskellPackages;
  inherit (haskellPackages) cabal;
  cabalInstall = haskellPackages.cabalInstall_1_18_0_3;
in cabal.mkDerivation (self: {
  pname = "hiak";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
  ];
  buildTools = [ cabalInstall ];
  enableSplitObjs = false;
})
