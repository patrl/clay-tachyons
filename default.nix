{ mkDerivation, base, clay, stdenv }:
mkDerivation {
  pname = "clay-tachyons";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base clay ];
  license = stdenv.lib.licenses.bsd3;
}
