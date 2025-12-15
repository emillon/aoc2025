{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
      {
        formatter = pkgs.nixpkgs-fmt;
        packages.default = pkgs.ocamlPackages.buildDunePackage {
          pname = "aoc2025";
          version = "n/a";
          src = ./.;
          buildInputs = with pkgs.ocamlPackages; [
            angstrom
            cmdliner
            diet
            ocamlgraph
            ppx_jane
            z3
          ];
        };
        devShells.default = pkgs.mkShell {
          inputsFrom = [ self.packages.${system}.default ];
          nativeBuildInputs = with pkgs.ocamlPackages; [
            merlin
            ocamlformat_0_28_1
          ];
        };
      });
}
