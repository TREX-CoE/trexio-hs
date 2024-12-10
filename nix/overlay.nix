{ trexioSrc }:
final: prev: {
  trexio = prev.trexio.overrideAttrs (oldAttrs: {
    src = prev.lib.cleanSourceWith {
      src = trexioSrc;
      filter =
        path: type:
        !(
          builtins.elem (builtins.baseNameOf path) [
            "docker"
            "helpers-debian"
            "ocaml"
            "rust"
          ]
          && type == "directory"
        );
    };
  });

  haskell = prev.haskell // 
  {
    packageOverrides = hfinal: hprev: 
    let ghcMajor = prev.lib.versions.major hprev.ghc.version;
        ghcMinor = prev.lib.versions.minor hprev.ghc.version;
    in prev.haskell.packageOverrides hfinal hprev // {
      trexio-hs = hfinal.callCabal2nix "trexio-hs" ../. { inherit (final) trexio; };
      # template-haskell = hprev.template-haskell_2_22_0_0;
    } // prev.lib.attrsets.optionalAttrs (ghcMajor == "9" && ghcMinor == "4") {
    };
  };
}
