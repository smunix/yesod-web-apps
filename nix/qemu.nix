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
        let amahoro-start-pre-script =
              pkgs.writeScriptBin "amahoro-start-pre-script" ''
                #!${pkgs.stdenv.shell}
                mkdir -pv /home/${user}/static/
                mkdir -pv /home/${user}/config/
              '';
        in
          {
            networking.hostName = hostname;
            networking.firewall.allowedTCPPorts = [ 22 3080 3000 ];
            environment.systemPackages = [
              apps.festhest
              apps.amahoro
            ];
            systemd.services.festhest = {
              enable = true;
              description = "festhest Webserver";
              wantedBy = [ "default.target" ];
              after = [ "network.target" ];
              serviceConfig = {
                Type = "simple";
                User = "${user}";
                WorkingDirectory = "~";
                ExecStart = "${apps.festhest}/bin/festhest";
              };
            };
            systemd.services.amahoro = {
              enable = true;
              description = "amahoro Webserver";
              wantedBy = [ "default.target" ];
              after = [ "network.target" ];
              serviceConfig = {
                Type = "simple";
                User = "${user}";
                WorkingDirectory = "~";
                ExecStartPre = "${amahoro-start-pre-script}/bin/amahoro-start-pre-script";
                ExecStart = "${apps.amahoro}/bin/amahoro";
              };
              environment = {
                YESOD_STATIC_DIR = "/home/${user}/static/";
              };
            };
            users = {
              mutableUsers = false;
              users = {
                root = {
                  password = "";
                };
                "${user}" = {
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
