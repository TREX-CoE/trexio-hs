# trexio-hs
Haskell bindings for the TREXIO library for reading, writing and storing wave function data in quantum chemistry and solid state physics.
For the original C-API see <https://github.com/TREX-CoE/trexio>

## Usage 
To build the Haskell bindings, the C library and headers need to be installed on your system and found by Cabal.
If they are in an exotic location, you may pass `--extra-lib-dirs=` and `--extra-include-dirs` to cabal.

### Nix
If you're using Nix, you can include these bindings as an overlay, e.g. such a `flake.nix`:

```nix
{
  description = "My Haskell project with TREXIO";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    trexio-hs.url = "github:TREX-CoE/trexio-hs";
  };

  outputs = 
  let system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ trexio-hs.overlays.default ];
      };
  in {
    packages."${system}".default = pkgs.haskellPackages.callCabal2Nix
      "my-project" ./. { };
  };
}

```

Of course, you can also simply build the project via `nix build github:TREX-CoE/trexio-hs`.

### Cabal
This package is on Hackage: <https://hackage.haskell.org/package/trexio-hs>
With plain nix, simply include `trexio-hs` in your `build-depends` field in your `cabal-file`.


## Documentation
Please have a look at the Haddocks. Build them via `cabal haddock`.
You may also have a look at [Hackage](https://hackage.haskell.org/package/trexio-hs).