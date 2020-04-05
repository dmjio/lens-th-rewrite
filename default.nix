{
  info ? builtins.fromJSON (builtins.readFile ./nixpkgs.json)
}:
with info;
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  config =
    { allowUnfree = true;
      packageOverrides = pkgs: {
        darwin = pkgs.darwin // {
          xcode = pkgs.darwin.xcode.overrideAttrs (drv: {
            outputHash = "ec9f78b948abe341000d31f21b66051741b71163d778702b3e2e01659d60e3d2";
          });
        };
      };
    };
  pkgs = import nixpkgs { inherit config; };
  overrides = self: super: {
    mkDerivation = args: super.mkDerivation (args // {
      enableLibraryProfiling = false;
      doCheck = false;
      doHaddock = false;
    });
    ghc = super.ghc.overrideAttrs (drv: {
      patchPhase = ''
        sed -i -e '4092,4093d' compiler/main/DynFlags.hs
      '';
    });
  };
  ghcARM = pkgs.pkgsCross.iphone64.haskell.packages.integer-simple.ghc865.override { inherit overrides; };
  call = pkgs.haskell.packages.ghc865.callCabal2nix;
  callARM = ghcARM.callCabal2nix;
  preprocessor = with pkgs.haskell.lib; justStaticExecutables (call "lens-th-rewrite" ./. {});
in
  preprocessor
