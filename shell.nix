with (import <nixpkgs> {});

mkShell {
  # buildInputs = [ cabal-install hpack haskell.compiler.ghc921 ispell ]
  #   ++ (with haskellPackages; [
  #     ghcid
  #     # Required by spacemacs haskell layer
  #     apply-refact hlint stylish-haskell hasktags hoogle
  #   ]);
  buildInputs = [ stack haskellPackages.ghcid ];
}
