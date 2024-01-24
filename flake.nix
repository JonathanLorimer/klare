{
  description = "klare";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: 
    let
      forAllSystems = function:
        nixpkgs.lib.genAttrs [
          "x86_64-linux"
          "aarch64-linux"
          "x86_64-darwin"
          "aarch64-darwin"
        ] (system: function rec {
          inherit system;
          compilerVersion = "ghc963";
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
            overrides = hfinal: hprev: {
              klare = hfinal.callCabal2nix "klare" ./. {};
            };
          };
        });
    in
    {
      # nix fmt
      formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

      # nix develop
      devShell = forAllSystems ({hsPkgs, pkgs, ...}:
        hsPkgs.shellFor {
          # withHoogle = true;
          packages = p: [
            p.klare
          ];
          buildInputs = with pkgs;
            [
              hsPkgs.haskell-language-server
              haskellPackages.cabal-install
              cabal2nix
              haskellPackages.ghcid
              haskellPackages.fourmolu
              haskellPackages.cabal-fmt
            ]
            ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}))
            ++
            ## Graphics deps
            [ 
              glfw
              glfw-wayland
              freeglut
              libGLU
            ];
        });

      # nix build
      packages = forAllSystems ({hsPkgs, ...}: {
          klare = hsPkgs.klare;
          default = hsPkgs.klare;
      });

      # You can't build the klare package as a check because of IFD in cabal2nix
      checks = {};

      # nix run
      apps = forAllSystems ({system, ...}: {
        klare = { 
          type = "app"; 
          program = "${self.packages.${system}.klare}/bin/klare"; 
        };
        playground = { 
          type = "app"; 
          program = "${self.packages.${system}.klare}/bin/playground-exe"; 
        };
        default = self.apps.${system}.klare;
      });
    };
}
