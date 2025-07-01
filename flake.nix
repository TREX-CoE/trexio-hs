{
  description = "Haskell bindings for TREXIO, the portable wave function file format";

  nixConfig = {
    allow-import-from-derivation = "true";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:NixOS/nixpkgs";

    trexio.url = "github:TREX-CoE/trexio";
  };

  outputs =
    {
      self,
      nixpkgs,
      trexio,
      flake-utils,
    }:
    let
      trexioOvl = import ./nix/overlay.nix;
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ trexioOvl trexio.overlays.default ];
        };
      in
      {
        packages.default = pkgs.haskellPackages.trexio-hs;

        devShells.default = pkgs.haskellPackages.shellFor {
          withHoogle = true;
          packages = p: [ p.trexio-hs ];
          buildInputs = with pkgs; [
            cabal-install
            haskell-language-server
            haskellPackages.fourmolu
            haskellPackages.haskell-ci
            hlint
            autoconf
            automake
          ];
        };

        formatter = pkgs.nixfmt-rfc-style;

        checks = self.packages."${system}" // {
          ghc96 = pkgs.haskell.packages.ghc96.trexio-hs;
          ghc98 = pkgs.haskell.packages.ghc98.trexio-hs;
          ghc910 = pkgs.haskell.packages.ghc910.trexio-hs;
          ghc912 = pkgs.haskell.packages.ghc912.trexio-hs;
        };
      }
    )
    // {
      overlays.default = trexioOvl;
    };
}
