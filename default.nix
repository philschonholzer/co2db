let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "2d182f0d0fa44d639e5478851ec0b3f3bddd573d";
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
            mmark
        ];
        otherDeps = p: with p; [
            ormolu
        ];
        projectPath = ./.;
    };
in
    haskellEnv
