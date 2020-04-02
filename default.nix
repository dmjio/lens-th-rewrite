{ pkgs ? import <nixpkgs> {} }: pkgs.haskellPackages.callCabal2nix "lens-th-rewrite" ./. {}
