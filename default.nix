{ mkDerivation, aeson, base, bytestring, containers, csv
, data-default, ghcjs-dom, lens, reflex, reflex-dom, stdenv, text
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "reflex-dom-lazy-grid";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring containers csv data-default ghcjs-dom lens
    reflex reflex-dom text time transformers unordered-containers
  ];
  homepage = "http://github.com/mulderr/reflex-dom-lazy-grid";
  description = "Lazy grid for Reflex";
  license = stdenv.lib.licenses.bsd3;
}
