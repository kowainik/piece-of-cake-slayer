let
  pkgs = import <nixpkgs> {};
in

pkgs.haskell.lib.buildStackProject {
  ghc = pkgs.haskell.compiler.ghc8107;
  name = "piece-of-cake-slayer";
  # System dependencies needed at compilation time
  buildInputs = [
    pkgs.zlib
    pkgs.postgresql
    pkgs.protobuf
  ];
}
