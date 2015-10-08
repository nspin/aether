{ mkDerivation, bencode, attoparsec, base, bigword, bytestring
, containers, lens, network, stdenv, stm, time
}:
mkDerivation {
  pname = "aether";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    bencode attoparsec base bigword bytestring containers lens network
    stm time
  ];
  homepage = "https://github.com/nickspinale/aether.git";
  description = "BitTorrent mainline DHT";
  license = stdenv.lib.licenses.mit;
}
