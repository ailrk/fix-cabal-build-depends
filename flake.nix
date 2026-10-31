{
  description = "Fix cabal build-depends";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/48652e9d5aea46e555b3df87354280d4f29cd3a3";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        fixCabalBuildDependsPackage = pkgs.haskellPackages.callCabal2nix "fix-cabal-build-depends" ./. {};

        devTools = with pkgs.haskellPackages; [
          ghc
          cabal-install
          haskell-language-server
        ];

        systemDeps = with pkgs; [
          zlib
          pkg-config
        ];
      in
        {
        packages.default = fixCabalBuildDependsPackage;

        apps.default = flake-utils.lib.mkApp {
          drv = fixCabalBuildDependsPackage;
          exePath = "/bin/fix-cabal-build-depends";
        };

        devShells.default = pkgs.mkShell {
          buildInputs = devTools ++ systemDeps;
        };
      });
}
