# Build with `nix-build`
{ nixpkgs ? import <nixpkgs> { config = {}; overlays = []; }
, compiler ? "ghc98"
}:
let
  hspkgs = (nixpkgs.pkgs.haskell.packages.${compiler}.override {
    # need to set all-cabal-hashes to latest version to allow the new hledger-lib version below.
    all-cabal-hashes = nixpkgs.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/3b714e51c078fcd31bdf3cb391d5349c5d8c1871.tar.gz";
      sha256 = "0bsxskkzlqq3cwfhs40403yinfrwkj2fbf66731d7lmgwp2rc7dy";
    };
    overrides = hself: hsuper: {
      # Can add/override packages here
      cassava-megaparsec = nixpkgs.haskell.lib.doJailbreak hsuper.cassava-megaparsec;
      hledger-lib = nixpkgs.haskell.lib.doJailbreak (hself.callHackage "hledger-lib" "1.41" {});
    };
  });
in
{
  activobank-hs =
    hspkgs.callCabal2nix "activobank-hs" ./. { };
}
