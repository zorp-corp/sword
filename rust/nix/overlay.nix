self: super: {
    libaes_siv = self.callPackage ./libaes_siv.nix { static = true; };
    urcrypt = self.callPackage ./urcrypt.nix { static = true; };
    openssl = super.openssl.override { static = true; };
    secp256k1 = super.secp256k1.overrideAttrs (attrs: { dontDisableStatic = true; });
    cryptopp = super.cryptopp.override { enableStatic = true; };
}
