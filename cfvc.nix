{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, containers, http-conduit, lens, mtl, optparse-applicative, random
, stdenv, time, word8, yaml
}:
mkDerivation {
  pname = "cfvc";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring case-insensitive containers http-conduit
    lens mtl optparse-applicative random time word8 yaml
  ];
  license = stdenv.lib.licenses.bsd3;
}
