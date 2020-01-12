{ mkDerivation, attoparsec, base, bytestring, case-insensitive
, containers, http-conduit, lens, mtl, random, stdenv
}:
mkDerivation {
  pname = "cfvc";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    attoparsec base bytestring case-insensitive containers http-conduit
    lens mtl random
  ];
  license = stdenv.lib.licenses.bsd3;
}
