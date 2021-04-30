let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        ref = "refs/tags/v0.9.0";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            url
        ];
        otherDeps = p: with p; [
            ormolu
        ];
        projectPath = ./.;
    };
in
    haskellEnv
