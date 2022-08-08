{
  description = "genType herc CI";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        system = system;
        overlays = [];
      };
      gentype-source = pkgs.mkYarnPackage {
        name = "gentype_source";
        src = ./.;
        packageJSON = ./package.json;
        yarnLock = ./yarn.lock;
        pkgConfig = {
          rescript = {
            buildInputs = with pkgs; [ which gcc_multi ];
            postInstall = ''
              echo "PATCHELF'ING RESCRIPT EXECUTABLES (INCL NINJA)"
              # Patching interpreter for linux/*.exe's
              THE_LD=$(patchelf --print-interpreter $(which mkdir))
              patchelf --set-interpreter $THE_LD linux/*.exe && echo "- patched interpreter for linux/*.exe's"

              # Replacing needed shared library for linux/ninja.exe
              THE_SO=$(find /nix/store/*/lib64 -name libstdc++.so.6 | head -n 1)
              patchelf --replace-needed libstdc++.so.6 $THE_SO linux/ninja.exe && echo "- replaced needed for linux/ninja.exe"
            '';
          };
        };
      };
      gentype = pkgs.stdenv.mkDerivation {
        name = "gentype";
        src = gentype-source + "/libexec/gentype/deps/gentype";
        buildInputs = with pkgs; [ ocaml dune_2 ];
        buildPhase = "dune build";
        installPhase = ''
          mkdir -p $out
          cp -r _build/default/src/GenType.exe $out
        '';
      };
    in {

      packages."${system}" = {
        gentype = gentype;
      };
      defaultPackage."${system}" = self.packages."${system}".gentype;
  };
}
