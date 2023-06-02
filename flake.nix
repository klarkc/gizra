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
          concurrent = pkgs.writeShellApplication {
            name = "concurrent";
            runtimeInputs = with pkgs; [
              concurrently
            ];
            text = ''
              concurrently\
                --color "auto"\
                --prefix "[{command}]"\
                --handle-input\
                --restart-tries 10\
                "$@"
            '';
          };
          tailwind = pkgs.writeShellApplication {
            name = "tailwind";
            runtimeInputs = with pkgs; [ nodejs ];
            text = ''npx tailwind "$@"'';
          };
          tailwind-watch = pkgs.writeShellApplication {
            name = "tailwind-watch";
            runtimeInputs = [ tailwind ];
            text = ''tailwind -c tailwind/tailwind.config.js -i ./tailwind/app.css -o static/app.css --watch "$@"'';
          };
          dev = (pkgs.writeShellApplication {
            name = "dev";
            runtimeInputs = with pkgs; gizra.buildInputs ++ [
              concurrent
              tailwind-watch
            ];
            text = ''
              make all
              concurrent \
                "tailwind-watch"\
                RunDevServer
            '';
          });
        in
        # Flake definition must follow gizra.cabal
        {
          packages.default = gizra;
          devShells.default = pkgs.mkShell {
            packages = [
              dev
              gizra
            ];
            inputsFrom = [
              gizra
            ];
            shellHook = ''
              alias echol='printf "\033[1;32m%s\033[0m\n" "$@"'
              alias echoi='printf "\033[1;34m[INFO] %s\033[0m\n" "$@"'
              echol "Welcome to gizra shell."
              echoi "Available commands: dev."
            '';
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
