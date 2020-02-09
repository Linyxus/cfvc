{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, containers, directory, http-conduit, lens, mtl
, optparse-applicative, pretty-simple, random, stdenv, time, word8
, yaml
}:
mkDerivation {
  pname = "cfvc";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring case-insensitive containers directory
    http-conduit lens mtl optparse-applicative pretty-simple random
    time word8 yaml
  ];
  license = stdenv.lib.licenses.bsd3;
}
