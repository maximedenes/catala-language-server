{
  description = "A language server for Catala based on LSP";

  inputs = {

    nixpkgs.url = "github:maximedenes/nixpkgs/init-catala-deps";
    catala.url = "github:maximedenes/catala/language-server";
    catala.inputs.nixpkgs.follows = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";

  };

  outputs = { self, nixpkgs, flake-utils, catala }:
    flake-utils.lib.eachDefaultSystem (system:

    let pkgs = import nixpkgs { inherit system; }; in

    let catala_pkg = catala.defaultPackage.${system}; in
    let ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14; in

    rec {

     packages.default = self.packages.${system}.catala-language-server;

     packages.catala-language-server =
       ocamlPackages.buildDunePackage {
         duneVersion = "3";
         pname = "catala-language-server";
         version = "0.0.1";
         src = ./.;
         buildInputs = [
            catala_pkg
          ]
          ++ (with ocamlPackages; [
           ocaml
           yojson
           findlib
           ppx_inline_test
           ppx_assert
           ppx_sexp_conv
           ppx_deriving
           sexplib
           ppx_yojson_conv
           jsonrpc
           lsp
           menhirLib
           sel
         ]);
       };

     devShells.default =
       with import nixpkgs { inherit system; };
       mkShell {
         buildInputs =
           self.packages.${system}.catala-language-server.buildInputs
           ++ (with ocamlPackages; [
             ocaml-lsp
           ]);
          propagatedBuildInputs = self.packages.${system}.catala-language-server.propagatedBuildInputs;
          nativeBuildInputs = self.packages.${system}.catala-language-server.nativeBuildInputs;
       };

  });
}
