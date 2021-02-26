{ system, version, nixpkgs, ... }:
final:
prev:
with final;
with haskellPackages;
with haskell.lib;
{
  qemu = recurseIntoAttrs ({
    apps = import ./overlays.nix { inherit system version; } final prev;
    vm = (import "${nixpkgs}/nixos" {
      inherit system;
      configuration = { config, pkgs, ... }: {
        
      };
    }).vm; 
  });
}
