{
  description = "A brainfuck interpreter";

  inputs = {
    nixpkgs = { url = "github:NixOS/nixpkgs/"; };
    flake-utils = { url = "github:numtide/flake-utils"; };
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          nix-filter.overlays.default
        ];
      };
    in

    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = pkgsFor system;
          filteredSrc =
            pkgs.nix-filter {
              root = ./.;
              include = [
                "src/"
                "package.yaml"
                "LICENSE"
                "benchmark/"
                "brainfucks/"
              ];
            };
        in
        rec {
          packages = rec {
            brainhask = pkgs.haskell.lib.justStaticExecutables
              pkgs.haskellPackages.brainhask;
            brainhask-bench-results = pkgs.stdenv.mkDerivation {
              name = "brainhask-bench-results";
              src = filteredSrc;
              buildPhase = ''
                ${brainhask}/bin/brainhask-bench --output benchmark-results.html
              '';
              installPhase = ''
                mkdir -p $out
                mv benchmark-results.html $out/
              '';
            };
          };

          defaultPackage = packages.brainhask;

          apps = {
            default = self.apps.brainhask;
            brainhask = {
              type = "app";
              program = "${packages.brainhask}/bin/brainhask";
            };
            brainhask-bench = {
              type = "app";
              program = "${packages.brainhask}/bin/brainhask-bench";
            };
          };


          devShells.default = pkgs.haskellPackages.shellFor {
            packages = p: [ packages.brainhask ];
            buildInputs = with pkgs; with pkgs.haskellPackages; [
              haskell-language-server
              cabal-install
              ghcid
              hpack
              hlint
              yamlfix
            ];
          };

          overlays = final: prev: {
            haskellPackages = prev.haskellPackages.override (old: {
              overrides = final.lib.composeExtensions
                (old.overrides or (_: _: { }))
                (self: super: {
                  brainhask = self.callCabal2nix "brainhask" filteredSrc { };
                }
                );
            });
          };
        });
}

