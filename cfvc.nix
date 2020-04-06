{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, containers, directory, http-conduit, lens, mtl
, optparse-applicative, pretty-simple, random, raw-strings-qq
, stdenv, time, word8, yaml
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
    raw-strings-qq time word8 yaml
  ];
  license = stdenv.lib.licenses.bsd3;
}
