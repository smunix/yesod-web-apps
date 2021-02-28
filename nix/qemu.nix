{ hostname, system, version, nixpkgs, ... }:
final:
prev:
with final;
with haskellPackages;
with haskell.lib;
{
  qemu = recurseIntoAttrs (rec {
    "${hostname}" = (import "${nixpkgs}/nixos" {
      inherit system;
      configuration = { config, pkgs, ... }:
        with (import ./overlays.nix { inherit system version; } final prev);
        let applySystemdScript = { name }:
              with pkgs;
              rec {
                workingDir = "/home/${name}";
                variables = {
                  YESOD_STATIC_DIR = "${workingDir}/static";
                  YESOD_CONFIG_DIR = "${workingDir}/config";
                };
                exec = {
                  start = rec {
                    pre-script = writeScriptBin "${name}" ''
                      #!${stdenv.shell}
                      mkdir -pv ${variables.YESOD_STATIC_DIR}
                      mkdir -pv ${variables.YESOD_CONFIG_DIR}
                    '';
                    pre = "${pre-script}/bin/${name}";
                    x = let app = apps."${name}";
                        in "${app}/bin/${name}";
                  };
                  
                };
              };
            mkSystemPackages = { name }:
              [ apps."${name}"
                (applySystemdScript { inherit name; }).exec.start.pre-script
              ];
            mkUser = { name, passwd ? "${name}" }:
              {
                "${name}" = {
                  isNormalUser = true;
                  createHome = true;
                  password = "${passwd}";
                  shell = fish;
                  extraGroups = [ "wheel" ];
                };
              };
            mkSystemdService = { name }:
              {
                "${name}" =
                  with (applySystemdScript { inherit name; });
                  {
                    enable = true;
                    description = "${name} Web app";
                    wantedBy = [ "default.target" ];
                    after = [ "network.target" ];
                    serviceConfig =
                      {
                        Type = "simple";
                        User = "${name}";
                        WorkingDirectory = "${workingDir}";
                        ExecStartPre = "${exec.start.pre}";
                        ExecStart = "${exec.start.x}";
                      };
                    environment = {
                      YESOD_STATIC_DIR = "${variables.YESOD_STATIC_DIR}";
                    };
                  };
              };
        in
          {
            networking.hostName = hostname;
            networking.firewall.allowedTCPPorts = [ 22 3080 3000 ];
            environment.systemPackages =
              []
              ++ (mkSystemPackages { name = "festhest"; })
              ++ (mkSystemPackages { name = "amahoro"; })
            ;
            systemd.services = {
              inherit (mkSystemdService { name = "festhest"; }) festhest;
              inherit (mkSystemdService { name = "amahoro"; }) amahoro;
            };
            users = {
              mutableUsers = false;
              users = {
                root = {
                  password = "root";
                };
                inherit (mkUser { name = "festhest"; }) festhest;
                inherit (mkUser { name = "amahoro"; }) amahoro;
              };
              extraGroups.vboxusers.members = [ "amahoro" ];
            };
            security.sudo = {
              enable = true;
              wheelNeedsPassword = false;
            };
            # Enable the KDE Desktop Environment.
            services.xserver.displayManager.sddm.enable = true;
            services.xserver.desktopManager.plasma5.enable = true;
            services.xserver = {
              windowManager = {
                awesome = {
                  enable = true;
                  luaModules = [ pkgs.luaPackages.luaposix ];
                };
              };
            };
            
            services.xserver.autorun = true;
            services.xserver.enable = true;

            virtualisation = {
              virtualbox.host.enable = true;
              graphics = true;
              # https://wiki.qemu.org/Documentation/Networking#Network_Basics
              qemu.networkingOptions = [
                # "-net nic,netdev=user.0,model=virtio"
                "-device virtio-net-pci,netdev=user.0 "
                "-netdev type=user,id=user.0\${QEMU_NET_OPTS:+,$QEMU_NET_OPTS},hostfwd=tcp::3080-:3080,hostfwd=tcp::3000-:3000"
              ];
            };
          };
    }).vm;
    "${hostname}-vm" = {
      type = "app";
      program =
        let vm = "${hostname}";
            script = writeScriptBin "${hostname}-vm" ''
             #!${stdenv.shell}
             exec ${vm}/bin/run-${hostname}-vm
           '';
        in "${script}/bin/${hostname}-vm";
    };
  });
}
