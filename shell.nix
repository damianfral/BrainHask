# This file was auto-generated by cabal2nix. Please do NOT edit manually!
{ haskellPackages ? (import <nixpkgs> {}).haskellPackages_ghcjs }:

with haskellPackages; cabal.mkDerivation (self: {
  pname = "brainhask";
  version = "0.1.0.0";
  src = "./.";
  isLibrary = false;
  isExecutable = true;
  enableProfilling = true;

  buildDepends = [ transformers free parsec pipes text either optparseApplicative criterion];
  meta = {
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
