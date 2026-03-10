{
  description = "Kando – a keyboard-first Kanban TUI for your terminal";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    crane.url = "github:ipetkov/crane";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      crane,
      rust-overlay,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];
      forAllSystems = fn: nixpkgs.lib.genAttrs systems fn;

      mkPkgs = system:
        import nixpkgs {
          inherit system;
          overlays = [ rust-overlay.overlays.default ];
        };

      mkCraneLib = pkgs:
        let
          toolchain = pkgs.rust-bin.stable.latest.minimal;
        in
        (crane.mkLib pkgs).overrideToolchain toolchain;
    in
    {
      packages = forAllSystems (system:
        let
          pkgs = mkPkgs system;
          craneLib = mkCraneLib pkgs;
          src = craneLib.cleanCargoSource ./.;
          commonArgs = {
            inherit src;
            strictDeps = true;
            nativeCheckInputs = [ pkgs.git ];
          };
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
          kando = craneLib.buildPackage (commonArgs // {
            inherit cargoArtifacts;
            meta = {
              description = "A keyboard-first Kanban TUI for your terminal";
              homepage = "https://github.com/krfl/kando";
              license = pkgs.lib.licenses.asl20;
              mainProgram = "kando";
            };
          });
        in
        {
          default = kando;
          inherit kando;
        }
      );

      checks = forAllSystems (system:
        let
          pkgs = mkPkgs system;
          craneLib = mkCraneLib pkgs;
          src = craneLib.cleanCargoSource ./.;
          commonArgs = {
            inherit src;
            strictDeps = true;
            nativeCheckInputs = [ pkgs.git ];
          };
          cargoArtifacts = craneLib.buildDepsOnly commonArgs;
        in
        {
          kando = self.packages.${system}.default;

          kando-clippy = craneLib.cargoClippy (commonArgs // {
            inherit cargoArtifacts;
            cargoClippyExtraArgs = "-- -D warnings";
          });

          kando-tests = craneLib.cargoTest (commonArgs // {
            inherit cargoArtifacts;
          });
        }
      );

      devShells = forAllSystems (system:
        let
          pkgs = mkPkgs system;
          toolchain = pkgs.rust-bin.stable.latest.default.override {
            extensions = [
              "rust-src"
              "rust-analyzer"
              "clippy"
            ];
          };
        in
        {
          default = pkgs.mkShell {
            packages = [
              toolchain
              pkgs.cargo-watch
              pkgs.git
            ];
          };
        }
      );

      overlays.default = final: _prev: {
        kando = self.packages.${final.system}.default;
      };
    };
}
