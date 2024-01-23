{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-utils.url = "flake-utils";
  };

  outputs = {self, fenix, flake-utils, nixpkgs}:
    let
      supportedSystems = ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"];
    in flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [(import ./nix/overlay.nix)];
        };
        parsedSystem = pkgs.lib.systems.parse.mkSystemFromString system;
      in {
        devShells.default = pkgs.mkShell {
          LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
          buildInputs = [
            (fenix.packages.${system}.complete.withComponents [
              "cargo"
              "clippy"
              "rustc"
              "rustfmt"
              "rust-src"
            ])
            pkgs.cargo-watch
            pkgs.bacon
            pkgs.iconv
            pkgs.llvmPackages.clang
            pkgs.pkg-config
            pkgs.urcrypt
            pkgs.glibc.static
          ] ++
            (nixpkgs.lib.lists.optional (parsedSystem.kernel.name != "darwin") pkgs.gdb); # nixpkgs won't build gdb for darwin
        };
      }
    );
}
