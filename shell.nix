{ pkgs ? import <nixpkgs> { } }:
let hsPkgs = pkgs.haskell.packages.ghc921;
in pkgs.mkShell {
  buildInputs = [ hsPkgs.ghc hsPkgs.cabal-install hsPkgs.haskell-language-server ];
}
