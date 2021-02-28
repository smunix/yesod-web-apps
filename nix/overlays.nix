{ system, version, ... }:
final:
_:
with final;
with haskellPackages;
with haskell.lib;
{
  apps = recurseIntoAttrs ({
    festhest = overrideCabal (addBuildTool (callCabal2nix "festhest" ../festhest {}) yesod-bin) (o: { version = o.version + "-" + version; });
    amahoro = overrideCabal (addBuildTool (callCabal2nix "amahoro" ../amahoro {}) yesod-bin) (o: { version = o.version + "-" + version; });
  });
}
