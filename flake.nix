{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, flake-utils, ... } @ inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = with inputs; [
            inputs.fenix.overlays.default
          ];
        };
        inherit (pkgs) fenix ra-flake;
        inherit (pkgs.lib) importTOML;

        # Rust toolchain
        rustChannel = (importTOML ./rust-toolchain).toolchain.channel;
        rustToolchain = fenix.toolchainOf {
          channel = rustChannel;
          sha256 = "sha256-DzNEaW724O8/B8844tt5AVHmSjSQ3cmzlU4BP90oRlY=";
        };
        rustWasmToolchain = fenix.targets.wasm32-unknown-unknown.toolchainOf {
          channel = rustChannel;
          sha256 = "sha256-DzNEaW724O8/B8844tt5AVHmSjSQ3cmzlU4BP90oRlY=";
        };
        # For development
        rust-dev = with fenix; combine (with rustToolchain; [
          defaultToolchain
          rustWasmToolchain.rust-std
          rust-src
          rust-analyzer
        ]);
      in
      {
        devShells.default = with pkgs; mkShell {
          nativeBuildInputs = [
            rust-dev
          ];
        };
      }
    );
}
