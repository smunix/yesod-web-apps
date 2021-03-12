{ system, version, ... }:
final:
_:
with final;
with haskellPackages;
with haskell.lib;
let random = callHackage "random" "1.2.0" {};
in
{
  apps = recurseIntoAttrs ({
    festhest = overrideCabal (addBuildTool (callCabal2nix "festhest" ../festhest { inherit random; }) yesod-bin) (o: { version = o.version + "-" + version; });
    amahoro = overrideCabal (addBuildTool (callCabal2nix "amahoro" ../amahoro {}) yesod-bin) (o: { version = o.version + "-" + version; });
    amakuru = overrideCabal (addBuildTool (callCabal2nix "amakuru" ../amakuru {}) yesod-bin) (o: { version = o.version + "-" + version; });
  });
}
