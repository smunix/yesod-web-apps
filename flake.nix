{
  description = "A very basic Yesod flake";
  inputs.flake-utils.url = "github:numtide/flake-utils/master";
  
  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      let version = "${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}"; 
          overlays = [ (import ./nix/overlays.nix { inherit system version; })
                       (import ./nix/qemu.nix { inherit system version nixpkgs; })
                     ];
      in
        with (import nixpkgs { inherit system overlays; });
        rec {
          packages = flattenTree ( recurseIntoAttrs {
            inherit apps qemu;
          });
      });
}
