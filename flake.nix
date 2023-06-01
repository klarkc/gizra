{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.utils.url = "github:ursi/flake-utils";
  inputs.ihp.url = "github:digitallyinduced/ihp";
  inputs.ihp.flake = false;
  inputs.npmlock2nix.url = "github:nix-community/npmlock2nix";
  inputs.npmlock2nix.flake = false;

  outputs = { self, utils, ihp, ... }@inputs:
    let
      compiler = "ghc944";
      mkGhcCompiler = import "${ihp}/NixSupport/mkGhcCompiler.nix";
    in
    utils.apply-systems
      {
        inherit inputs;
        # adapt IHP NixSupport/make-nixpkgs-from-options.nix to flake
        make-pkgs = (system: import inputs.nixpkgs {
          inherit system;
          config = {
            packageOverrides = pkgs: rec {
              haskell = pkgs.haskell // {
                packages = pkgs.haskell.packages // {
                  "${compiler}" = mkGhcCompiler {
                    inherit pkgs ihp;
                    ghcCompiler = pkgs.haskell.packages.${compiler};
                  };
                };
              };
            };
          };
        });
      }
      ({ pkgs, system, ... }:
        let
          projectPath = ./.;
          ihp-nix = import "${ihp}/NixSupport";
          npmlock2nix = import inputs.npmlock2nix {
            inherit pkgs;
          };
          node_modules = npmlock2nix.v2.node_modules {
            src = projectPath;
            nodejs = pkgs.nodejs;
          };
          gizra = (ihp-nix {
            inherit ihp projectPath pkgs compiler;
            haskellDeps = p: with p; [
              cabal-install
              base
              wai
              text
              hlint
              p.ihp
            ];
            otherDeps = p: with p; [
              # Native dependencies, e.g. imagemagick
              imagemagick
              nodejs
            ];
          }).overrideAttrs (attrs: {
            # install npm packages
            postConfigure = ''
              ln -s ${node_modules}/node_modules node_modules
            '';
          });
        in
        # Flake definition must follow gizra.cabal
        {
          packages.default = gizra;
          devShells.default = pkgs.mkShell {
            inputsFrom = [
              gizra
            ];
          };
          checks.output = pkgs.runCommand "gizra-output" { }
            ''
              echo ${gizra} > $out
            '';
        });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    # This sets the flake to use nix cache.
    # Nix should ask for permission before using it,
    # but remove it here if you do not want it to.
    extra-substituters = [
      "https://klarkc.cachix.org?priority=99"
      "https://cache.nixos.org"
      "https://digitallyinduced.cachix.org"
    ];
    extra-trusted-public-keys = [
      "klarkc.cachix.org-1:R+z+m4Cq0hMgfZ7AQ42WRpGuHJumLLx3k0XhwpNFq9U="
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "digitallyinduced.cachix.org-1:y+wQvrnxQ+PdEsCt91rmvv39qRCYzEgGQaldK26hCKE="
    ];
    allow-import-from-derivation = "true";
  };
}
