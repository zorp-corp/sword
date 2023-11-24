{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {self, fenix, flake-utils, nixpkgs}:
    let supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
    in flake-utils.lib.eachSystem supportedSystems
    (system:
      let pkgs = import nixpkgs { inherit system; overlays = [(import ./nix/overlay.nix)]; };
      in { devShells.default = pkgs.mkShell {
          buildInputs = [
            (fenix.packages.${system}.complete.withComponents [
              "cargo"
              "clippy"
              "rustc"
              "rustfmt"
              "rust-src"
            ])
            pkgs.autoconf-archive
            pkgs.automake
            pkgs.cargo-watch
            pkgs.cmake
            (pkgs.lib.optional (system == "x86_64-linux") pkgs.gdb)
            pkgs.iconv
            pkgs.openssl
            pkgs.pkg-config
            pkgs.urcrypt
          ];
        };
      }
    );
}

