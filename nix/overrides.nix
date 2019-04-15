{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  generics-bench = (
    with rec {
      generics-benchSource = pkgs.lib.cleanSource ../.;
      generics-benchBasic  = self.callCabal2nix "generics-bench" generics-benchSource { };
    };
    overrideCabal generics-benchBasic (old: {
    })
  );
}
