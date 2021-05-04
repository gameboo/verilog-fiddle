{ pkgs ? import <nixpkgs> {} } :
with pkgs;
mkShell {
  name = "verilog-fiddle-dev-env";
  buildInputs = [
    (ghc.withPackages (hPkgs: with hPkgs; [ regex-tdfa ]))
  ];
}
