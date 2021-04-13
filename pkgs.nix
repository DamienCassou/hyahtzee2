# Copied from:
# - https://jonascarpay.com/posts/2021-01-28-haskell-project-template.html
# - https://github.com/jonascarpay/template-haskell
let
  haskellNix =
    let
      # 2021-04-12
      commit = "e1a7bab50735b19c8ba1db4e4b0db672a825d7cd";
      sha256 = "1zqhjvc2qzrid9bm4a5qv3mmqmybkx4j4bkqp4w5hifyq8fhhqks";
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
      compiler-nix-name = "ghc8102";
    };
  };
in
import pkgsSrc (pkgsArgs // {
  overlays = pkgsArgs.overlays ++ [ overlay ];
})
