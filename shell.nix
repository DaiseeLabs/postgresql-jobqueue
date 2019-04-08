{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, heavy-logger, hpack
      , hspec, hspec-core, monad-control, optparse-applicative
      , postgresql-simple, stdenv, uuid, uuid-quasi
      }:
      mkDerivation {
        pname = "postgresql-jobqueue";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring heavy-logger monad-control optparse-applicative
          postgresql-simple uuid
        ];
        libraryToolDepends = [ hpack ];
        testHaskellDepends = [
          async base hspec hspec-core postgresql-simple uuid uuid-quasi
        ];
        preConfigure = "hpack";
        license = stdenv.lib.licenses.unfree;
        hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
