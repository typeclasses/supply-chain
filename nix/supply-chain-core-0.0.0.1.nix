{ mkDerivation, base, hspec, lib }:
mkDerivation {
  pname = "supply-chain-core";
  version = "0.0.0.1";
  sha256 = "ffcbd5cbfa1741734f7bdf774173bec8f2d97a57f1913d19c9ca6fa6b078a9a3";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base hspec ];
  homepage = "https://github.com/typeclasses/supply-chain-core";
  description = "Composable request-response pipelines";
  license = lib.licenses.asl20;
}
