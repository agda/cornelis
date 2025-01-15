{
  description = "Agda mode for vim";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = { url = "github:edolstra/flake-compat"; flake = false; };
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      name = "cornelis";
      # Update `./.github/workflows/nix.yml` if changed.
      # `ghc902` excluded due to build issues.
      ghcVersions = map (v: "ghc${v}") [ "810" "92" "94" "96" "98" ];
      # Ensure resolver in `./stack.yaml` is in sync with `defaultGhcVersion`.
      defaultGhcVersion = "ghc96";
    in
    {
      overlays = {
        ${name} = final: prev: {
          haskell = prev.haskell // {
            packageOverrides = final.lib.composeExtensions
              prev.haskell.packageOverrides
              (hfinal: hprev: let
                inherit (final.haskell.lib.compose) enableSeparateBinOutput addTestToolDepends;
                inherit (final.lib) pipe;
              in {
                # Put binaries into separate output "bin" to reduce closure size.
                # https://nixos.org/manual/nixpkgs/stable/#haskell-packaging-helpers
                ${name} = pipe (hfinal.callCabal2nix name ./. { }) [
                  enableSeparateBinOutput
                  (addTestToolDepends [ final.agda final.neovim ])
                ];
              });
          };

          ${name} = final.haskell.packages.${defaultGhcVersion}.${name};

          vimPlugins = prev.vimPlugins.extend (pfinal: _: {
            ${name} = final.vimUtils.buildVimPlugin {
              pname = name;
              inherit (final.${name}) version;
              src = ./.;
              dependencies = builtins.attrValues {
                inherit (pfinal) nvim-hs-vim vim-textobj-user;
              };
            };
          });
        };

      };
    } // inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.${name} ];
        };
        agda = pkgs.agda.withPackages (p: [ p.standard-library ]);
      in
      rec {
        packages = {
          inherit agda;
          ${name} = pkgs.${name};
          "${name}-vim" = pkgs.vimPlugins.${name};
          default = pkgs.${name};

          # Create `cornelis` package for all `ghcVersions`.
        } // builtins.listToAttrs (map
          (v: { name = "${name}-${v}"; value = pkgs.haskell.packages.${v}.${name}; })
          ghcVersions
        );
        defaultPackage = packages.default;
        app = {
          cornelis = inputs.flake-utils.lib.mkApp {
            inherit name; drv = packages.default;
          };
          default = app.cornelis;
        };
        defaultApp = app.default;
        devShells.default = pkgs.callPackage ./nix/dev-shells.nix { };
      }
    );
}
