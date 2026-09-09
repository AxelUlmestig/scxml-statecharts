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
      shellFor = { compiler, tools ? [ ] }:
        pkgs.mkShell {
          name = "scxml-statecharts-${compiler}";
          buildInputs = [
            pkgs.haskell.packages.${compiler}.ghc
            pkgs.cabal-install
            # xml-conduit reaches the C zlib through conduit-extra and
            # streaming-commons, and the Haskell zlib package links it rather
            # than bundling it. It locates the library with pkg-config and
            # falls back to a bare -lz where that executable is missing, so
            # both are needed: without them the build stops at zlib's
            # configure step with "Missing (or bad) C library: z".
            pkgs.pkg-config
            pkgs.zlib
          ] ++ tools;
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
