let
  pkgs = import <nixpkgs> { };

in
  { cfvc = pkgs.haskellPackages.callPackage ./cfvc.nix { };
  }
