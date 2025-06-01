{
  description = "Zin Flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs =
    { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          ocamlPackages.ocaml
          ocamlPackages.dune_3
          ocamlPackages.findlib
          ocamlPackages.utop
          ocamlPackages.base
          ocamlPackages.ppx_deriving
          ocamlPackages.ppxlib
          ocamlPackages.ounit
          ocamlPackages.odoc
          ocamlformat
          ocamlPackages.ocaml-lsp
          ocamlPackages.lsp
        ];
      };

    };
}
