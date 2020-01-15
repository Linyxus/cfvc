{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, containers, http-conduit, lens, mtl, random, stdenv, time, word8
}:
mkDerivation {
  pname = "cfvc";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring case-insensitive containers http-conduit
    lens mtl random time word8
  ];
  license = stdenv.lib.licenses.bsd3;
}
