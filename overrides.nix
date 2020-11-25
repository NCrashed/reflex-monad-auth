# Here you can put overrides of dependencies
{ reflex-platform ? (import ./reflex-platform.nix {}), ... }:
let
  pkgs = reflex-platform.nixpkgs;
  lib = pkgs.haskell.lib;
in (self: super: let
  # Overrides from cabal2nix files
  derivationsOverrides = lib.packagesFromDirectory { directory = ./derivations; } self super;
  in derivationsOverrides // {
    reflex-monad-auth = lib.addExtraLibraries (lib.enableCabalFlag super.reflex-monad-auth "examples") (with self; [
        raw-strings-qq
        reflex-dom
      ]);
  }
)
