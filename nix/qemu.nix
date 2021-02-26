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
        let user = "festhest"
        in
          {
            networking.hostName = user;
            networking.firewall.allowedTCPPorts = [ 22 80 ];
            environment.systemPackages = [ apps.festhest ];
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
            users = {
              mutableUsers = false;
              users = {
                root = {
                  password = "";
                };
                "${user}" = {
                  isSystemUser = true;
                  password = "";
                };
              };
              extraUsers = {
                "${user}" = {
                  group = "wheel";
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
