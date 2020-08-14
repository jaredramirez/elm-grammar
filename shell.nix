{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    buildInputs = [
      pkgs.fish
      pkgs.rustc
      pkgs.cargo
      pkgs.cargo-watch
    ];
  }
