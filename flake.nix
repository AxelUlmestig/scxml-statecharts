{
  description = "scxml-statecharts development shells";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { nixpkgs, flake-utils, ... }: flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };

      # cabal-install is a tool rather than a library, so one build of it serves
      # every compiler below.
      #
      # zlib is a C library rather than a Haskell one: xml-conduit reaches it
      # through conduit-extra and streaming-commons, and the Haskell zlib
      # package it ends up building links against it. Without it in the shell,
      # cabal compiles everything and then fails at the link with
      # "ld.gold: cannot find -lz".
      shellFor = { compiler, tools ? [ ] }:
        pkgs.mkShell {
          name = "scxml-statecharts-${compiler}";
          buildInputs = [
            pkgs.haskell.packages.${compiler}.ghc
            pkgs.cabal-install
            pkgs.zlib
          ] ++ tools;

          # Linking finds libz through buildInputs, but GHC's own runtime
          # linker does not: it loads the zlib package whenever it loads the
          # package, which is every repl session and every module with a
          # Template Haskell splice, and it looks on LD_LIBRARY_PATH.
          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.zlib ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
          '';
        };

      # Only the default compiler has a cached haskell-language-server. Asking
      # for it on any other one builds the whole HLS dependency chain from
      # source, which takes hours, so the release shells below go without.
      hlsFor = compiler: [ pkgs.haskell.packages.${compiler}.haskell-language-server ];

    in
    {
      devShells = {
        # The everyday shell, and what direnv loads: the GHC named in the cabal
        # file's tested-with field.
        default = shellFor { compiler = "ghc9103"; tools = hlsFor "ghc9103"; };

        # The GHCs a Hackage or Stackage builder is likely to pick. Used to
        # check a release, not to work in:
        #   nix develop .#ghc9124 --command cabal test
        ghc9124 = shellFor { compiler = "ghc9124"; };
        ghc9141 = shellFor { compiler = "ghc9141"; };
      };
    }
  );
}
