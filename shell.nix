# To have ormolu automatically format imports using the ImportQualifiedPost syntax,
# comment out `ormolu` from the `tools` section, and uncomment `buildInputs`
let
  pkgs = import ./pkgs.nix;
  hsPkgs = pkgs.hsPkgs;
in
hsPkgs.shellFor {
  withHoogle = true;
  tools = {
    cabal = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
    ormolu = "latest";
    weeder = "latest";
  };
  exactDeps = true;
}
