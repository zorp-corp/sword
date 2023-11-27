{ stdenv, fetchFromGitHub, autoreconfHook, pkg-config, openssl, cryptopp, secp256k1, libaes_siv }:
let rev = "375fa7e6a730d8aa517ca981b2b7b505bf4e1103";
in stdenv.mkDerivation {
  pname = "urcrypt";
  version = "git-${rev}";
  src = fetchFromGitHub {
    inherit rev;
    owner = "urbit";
    repo = "urcrypt";
    hash = "sha256:1c3cqmwr5mys4v9y0834hxqfr6aynm2gav7730bjzfvrdc21ijqa";
  };

  preConfigure = ''
    ./autogen.sh
    '';

  nativeBuildInputs = [autoreconfHook pkg-config];

  buildInputs = [openssl cryptopp secp256k1 libaes_siv];
}
