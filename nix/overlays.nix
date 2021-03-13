{ system, version, ... }:
self:
super:
with self;
with super.haskellPackages.extend(hself: hsuper: {
  random = super.haskellPackages.callHackage "random" "1.2.0" {};
});
with haskell.lib;
{
  apps = recurseIntoAttrs ({
    festhest =overrideCabal (addBuildTool (callCabal2nix "festhest" ../festhest {}) yesod-bin) (o: { version = o.version + "-" + version; });
    amahoro = overrideCabal (addBuildTool (callCabal2nix "amahoro" ../amahoro {}) yesod-bin) (o: { version = o.version + "-" + version; });
    amakuru = overrideCabal (addBuildTool (callCabal2nix "amakuru" ../amakuru {}) yesod-bin) (o: { version = o.version + "-" + version; });
  });
}
