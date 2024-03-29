{
  description = "A brainfuck interpreter";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , nix-filter
    , pre-commit-hooks
    , ...
    }:

    let
      pkgsFor = system: import nixpkgs {
        inherit system;
        overlays = [ self.overlays.default ];
      };
      filteredSrc = nix-filter.lib {
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
    {
      overlays.default = final: prev:
        with final.haskell.lib;
        {
          haskellPackages = prev.haskellPackages.override (old: {
            overrides = final.lib.composeExtensions
              (old.overrides or (_: _: { }))
              (self: super: {
                sydtest = unmarkBroken (dontCheck super.sydtest);
                brainhask = self.generateOptparseApplicativeCompletions
                  [ "brainhask" ]
                  (self.callCabal2nix "brainhask" filteredSrc { });
              }
              );
          });
        }
      ;
    } //

    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = pkgsFor system;
      precommitCheck = pre-commit-hooks.lib.${system}.run {
        src = ./.;
        hooks = {
          actionlint.enable = true;
          hlint.enable = true;
          hpack.enable = true;
          markdownlint.enable = true;
          nil.enable = true;
          nixpkgs-fmt.enable = true;
          ormolu.enable = true;
          statix.enable = true;
        };
      };
    in
    rec {


      packages.brainhask = pkgs.haskellPackages.brainhask;
      packages.brainhask-bench-results = pkgs.stdenv.mkDerivation {
        name = "brainhask-bench-results";
        src = filteredSrc;
        buildPhase = "${apps.brainhask-bench.program} --output benchmark-results.html";
        installPhase = ''
          mkdir -p $out
          mv benchmark-results.html $out/
        '';
      };
      packages.default = packages.brainhask;

      apps.brainhask = flake-utils.lib.mkApp {
        drv = pkgs.haskell.lib.justStaticExecutables (
          packages.brainhask.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
          })
        );
      };

      apps.brainhask-bench = flake-utils.lib.mkApp {
        name = "brainhask-bench";
        drv = pkgs.haskell.lib.justStaticExecutables (
          packages.brainhask.overrideAttrs (oldAttrs: {
            configureFlags = oldAttrs.configureFlags ++ [ "--ghc-option=-O2" ];
          })
        );
      };
      apps.default = apps.brainhask;


      devShells.default = pkgs.haskellPackages.shellFor {
        packages = p: [ packages.brainhask ];
        buildInputs = with pkgs; with pkgs.haskellPackages; [
          actionlint
          cabal-install
          ghcid
          haskell-language-server
          hlint
          nil
          nixpkgs-fmt
          ormolu
          statix
        ];
        inherit (precommitCheck) shellHook;
      };
    });
  nixConfig = {
    extra-substituters = "https://opensource.cachix.org";
    extra-trusted-public-keys = "opensource.cachix.org-1:6t9YnrHI+t4lUilDKP2sNvmFA9LCKdShfrtwPqj2vKc=";
  };
}

