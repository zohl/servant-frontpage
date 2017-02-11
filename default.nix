{ mkDerivation, base, servant-server, stdenv, time }:
mkDerivation {
  pname = "servant-frontpage";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base servant-server time ];
  description = "Serving html page from file with caching and detecting changes in the file";
  license = stdenv.lib.licenses.bsd3;
}
