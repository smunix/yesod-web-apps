{ system, version, ... }:
self:
super:
with self;
with super.haskellPackages.extend(hself: hsuper:
  with haskellPackages; with haskell.lib;
    let lpkgs =
          [
            "authenticate"
            "conduit"
            "conduit-extra"
            "foldl"
            "hpack"
            "html-conduit"
            "http-conduit"
            "http-reverse-proxy"
            "mono-traversable"
            "persistent"
            "persistent-sqlite"
            "persistent-template"
            "project-template"
            "simple-sendfile"
            "xml-conduit"
            "yaml"
            "yesod"
            "yesod-auth"
            "yesod-core"
            "yesod-bin"
            "yesod-form"
            "yesod-persistent"
          ];
        lattrs = builtins.listToAttrs (map (x: { name = x; value = dontHaddock (dontCheck haskellPackages."${x}"); }) lpkgs); 
    in {
      random = callHackage "random" "1.2.0" {};
      hashable = callCabal2nix "hashable" (fetchFromGitHub {
        owner = "haskell-unordered-containers";
        repo = "hashable";
        rev = "34096a6562df0fd98e21b907c3d256f0d1a79b1e";
        sha256 = "0g7vnyj02pmcnliaagy8fsnbijca041nhm4q0hx07zasg9sfb90h";
      }) {};
    } // lattrs);
with haskell.lib;
{
  apps = recurseIntoAttrs ({
    festhest =overrideCabal (addBuildTool (dontCheck (callCabal2nix "festhest" ../festhest {})) yesod-bin) (o: { version = o.version + "-" + version; });
    amahoro = overrideCabal (addBuildTool (callCabal2nix "amahoro" ../amahoro {}) yesod-bin) (o: { version = o.version + "-" + version; });
    amakuru = overrideCabal (addBuildTool (callCabal2nix "amakuru" ../amakuru {}) yesod-bin) (o: { version = o.version + "-" + version; });
  });
}
