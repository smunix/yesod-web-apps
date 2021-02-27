{ system, version, nixpkgs, ... }:
final:
prev:
with final;
with haskellPackages;
with haskell.lib;
let user = "king";
    hostname = "olhajwon";
in
{
  qemu = recurseIntoAttrs ({
    "${hostname}" = (import "${nixpkgs}/nixos" {
      inherit system;
      configuration = { config, pkgs, ... }:
        with (import ./overlays.nix { inherit system version; } final prev);
        {
          networking.hostName = hostname;
          networking.firewall.allowedTCPPorts = [ 22 80 ];
          environment.systemPackages = [
            apps.festhest
            apps.amahoro
          ];
          systemd.services.festhest = {
            description = "festhest Webserver";
            wantedBy = [ "default.target" ];
            after = [ "network.target" ];
            serviceConfig = {
              Type = "forking";
              User = "root";
              ExecStart = "${apps.festhest}/bin/festhest";
            };
          };
          systemd.services.amahoro = {
            description = "amahoro Webserver";
            wantedBy = [ "default.target" ];
            after = [ "network.target" ];
            serviceConfig = {
              Type = "forking";
              User = "root";
              ExecStart = "${apps.amahoro}/bin/amahoro";
            };
          };
          users = {
            mutableUsers = false;
            users = {
              root = {
                password = "";
              };
              king = {
                isNormalUser = true;
                createHome = true;
                password = "";
                extraGroups = [ "wheel" ];
              };
            };
            extraUsers = {
              king = {
                shell = pkgs.fish;
              };
            };
          };
          security.sudo = {
            enable = true;
            wheelNeedsPassword = false;
          };
          virtualisation = {
            graphics = false;
          };
        };
    }).vm; 
  });
}
