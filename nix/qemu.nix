{ system, version, nixpkgs, ... }:
final:
prev:
with final;
with haskellPackages;
with haskell.lib;
{
  qemu = recurseIntoAttrs ({
    vm = (import "${nixpkgs}/nixos" {
      inherit system;
      configuration = { config, pkgs, ... }:
        with (import ./overlays.nix { inherit system version; } final prev);
        {
          networking.hostName = "festhest";
          networking.firewall.allowedTCPPorts = [ 22 80 ];
          environment.systemPackages = [ apps.festhest ];
          systemd.services.festhest = {
            description = "festhest Webserver";
            wantedBy = [ "multi-user.agent" ];
            serviceConfig = {
              ExecStart = "${apps.festhest}/bin/festhest";
            };
          };
          users = {
            mutableUsers = false;
            users = {
              root.password = "";
              "festhest".isSystemUser = true;
            };
          };
          virtualisation = {
            graphics = false;
          };
        };
    }).vm; 
  });
}
