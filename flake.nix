{
  description = "rubik";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, flake-utils, rust-overlay, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };
        rustVersion =
          pkgs.rust-bin.stable."1.74.0"; # Choose your desired Rust version
        runtimeDependencies = with pkgs; [ wayland libxkbcommon ];
        buildDependencies = with pkgs; [
          zlib
          bzip2
          libpng
          harfbuzz
          brotli
          fontconfig
        ];
        rpath = with pkgs; lib.makeLibraryPath runtimeDependencies;
      in rec {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          version = "0.1.0";
          pname = "rubik";
          cargoLock.lockFile = ./Cargo.lock;
          nativeBuildInputs = with pkgs; [
            cmake
            rustVersion.default
            pkg-config
          ];
          buildInputs = buildDependencies;
          postFixup = ''
            patchelf $out/bin/rubik --add-rpath ${rpath}
          '';

          src = pkgs.lib.cleanSourceWith {
            filter = name: type:
              let baseName = baseNameOf (toString name);
              in !(builtins.elem baseName [ "flake.nix" "flake.lock" ]);
            src = pkgs.lib.cleanSource ./.;
            name = "rubik-src";
          };
        };

        devShells = {
          default = pkgs.mkShell {
            buildInputs = [ rustVersion.default ];
            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath buildDependencies;
          };
          try = pkgs.mkShell { buildInputs = [ packages.default ]; };
        };
      });
}
