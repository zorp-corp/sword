{ sources ? import ./nix/sources.nix,
  pkgs ? import sources.nixpkgs {
    overlays = [ (import "${sources.fenix}/overlay.nix") (import ./nix/overlay.nix) ];
  }
}:
pkgs.mkShell {
  LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
  packages = with pkgs; [
    (fenix.stable.withComponents [
      "cargo"
      "clippy"
      "rustc"
      "rustfmt"
      "rust-src"
    ])
    cargo-watch
    gdb
    urcrypt
    pkg-config
    llvmPackages.clang
  ];
}
