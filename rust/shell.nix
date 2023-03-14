{ sources ? import ./nix/sources.nix,
  pkgs ? import sources.nixpkgs {
    overlays = [ (import "${sources.fenix}/overlay.nix") ];
  }
}:
pkgs.mkShell {
  packages = with pkgs; [
    (fenix.stable.withComponents [
      "cargo"
      "clippy"
      "rustc"
      "rustfmt"
      "rust-src"
    ])
    cargo-watch
  ];
}
