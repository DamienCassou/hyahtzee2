# Copied from:
# - https://jonascarpay.com/posts/2021-01-28-haskell-project-template.html
# - https://github.com/jonascarpay/template-haskell
let
  haskellNix =
    let
      # 2021-04-17
      commit = "04c5b1e5662a8409da55752a84d5316b1bd333ea";
      sha256 = "14a0rlykbk0vmy8hfk59ca96fppd6rv3308yr1m45qzpah77mahy";
    in
    import
      (builtins.fetchTarball {
        url = "https://github.com/input-output-hk/haskell.nix/archive/${commit}.tar.gz";
        inherit sha256;
      })
      { };
  pkgsSrc = haskellNix.sources.nixpkgs-2009;
  pkgsArgs = haskellNix.nixpkgsArgs;
  overlay = self: _: {
    hsPkgs = self.haskell-nix.project {
      src = self.haskell-nix.haskellLib.cleanGit {
        src = ./.;
        name = "hyahtzee2";
      };
      compiler-nix-name = "ghc8104";
    };
  };
in
import pkgsSrc (pkgsArgs // {
  overlays = pkgsArgs.overlays ++ [ overlay ];
})
