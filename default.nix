let
    ihp = builtins.fetchGit {
        url = "https://github.com/digitallyinduced/ihp.git";
        rev = "7b9cdfbb9b341cdee3d7d0adf61ba1dc326ec296";
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
