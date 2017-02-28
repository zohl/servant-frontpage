{ mkDerivation, base, bytestring, directory, exceptions, hspec
, hspec-wai, http-media, servant, servant-server, stdenv, time, wai
, wai-extra
}:
mkDerivation {
  pname = "servant-frontpage";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring directory exceptions http-media servant
    servant-server time
  ];
  testHaskellDepends = [
    base bytestring directory hspec hspec-wai servant servant-server
    wai wai-extra
  ];
  description = "Serving html page from file with caching and detecting changes in the file";
  license = stdenv.lib.licenses.bsd3;
}
