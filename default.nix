let
  reflex-platform = import ./reflex-platform.nix {};
in reflex-platform.project ({ pkgs, ... }: {
  packages = {
    reflex-monad-auth = ./.;
  };
  shells = {
    ghcjs = ["reflex-monad-auth"];
    ghc = ["reflex-monad-auth"];
  };
  overrides = import ./overrides.nix { inherit reflex-platform; };
})
