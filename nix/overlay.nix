final: prev: {
  haskell = prev.haskell // {
    packageOverrides =
      hfinal: hprev:
      let
        ghcMajor = prev.lib.versions.major hprev.ghc.version;
        ghcMinor = prev.lib.versions.minor hprev.ghc.version;
      in
      prev.haskell.packageOverrides hfinal hprev
      // {
        trexio-hs = hfinal.callCabal2nix "trexio-hs" ../. { inherit (final) trexio; };
      };
  };
}
