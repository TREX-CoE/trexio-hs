{
  description = "Haskell bindings for TREXIO, the portable wave function file format";

  nixConfig = {
    allow-import-from-derivation = "true";
  };

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";

    nixpkgs.url = "github:NixOS/nixpkgs";

    trexio = {
      url = "github:TREX-CoE/trexio";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      trexio,
      flake-utils,
    }:
    let
      trexioOvl = import ./nix/overlay.nix {
        trexioSrc = trexio;
      };
    in
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ trexioOvl ];
        };
      in
      {
        packages.default = pkgs.haskellPackages.trexio-hs;

        devShells.default = pkgs.haskellPackages.shellFor {
          withHoogle = true;
          packages = p: [ p.trexio-hs ];
          buildInputs = with pkgs; [
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
          # ghc92 = pkgs.haskell.packages.ghc92.trexio-hs;
          # ghc94 = pkgs.haskell.packages.ghc94.trexio-hs;
          ghc96 = pkgs.haskell.packages.ghc96.trexio-hs;
          ghc98 = pkgs.haskell.packages.ghc98.trexio-hs;
          # ghc910 = pkgs.haskell.packages.ghc910.trexio-hs;
        };
      }
    )
    // {
      overlays.default = trexioOvl;
    };
}
