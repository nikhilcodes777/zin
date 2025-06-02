{
  description = "Zin - A cool interpreted language made in OCaml";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

      in
      {

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ocamlPackages.ocaml
            ocamlPackages.dune_3
            ocamlPackages.findlib
            ocamlPackages.utop
            ocamlPackages.base
            ocamlPackages.ppx_deriving
            ocamlPackages.ppxlib
            ocamlPackages.ounit
            ocamlPackages.cmdliner
            ocamlPackages.odoc
            ocamlformat
            ocamlPackages.ocaml-lsp
            ocamlPackages.lsp
          ];

        };

        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "zin";
          version = "v1.0.0";
          meta = {
            homepage = "https://github.com/nikhilcodes777/zin";
            description = "Zin - A cool interpreted language made in OCaml";
            license = pkgs.lib.licenses.mit;
          };
          src = self;
          buildInputs = with pkgs.ocamlPackages; [
            cmdliner
            base
            ppx_deriving
            ppxlib
          ];

        };
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/zin";
        };
      }
    );

}
