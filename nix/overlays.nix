{ system, version, ... }:
final:
_:
with final;
with haskellPackages;
with haskell.lib;
{
  apps = recurseIntoAttrs ({
    festhest = overrideCabal (callCabal2nix "festhest" ../festhest {}) (o: { version = o.version + "-" + version; });
    amahoro = overrideCabal (callCabal2nix "amahoro" ../amahoro {}) (o: { version = o.version + "-" + version; });
  });
}
