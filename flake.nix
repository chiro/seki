{
  description = "Pinned seki CI and real-river integration environment";

  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/55bfb4d7437bd4e09ab7cba58a43bd68eb310044";
  inputs.nixpkgs-ghc.url =
    "github:NixOS/nixpkgs/3ec636362e1816bc2f0888fa0fa2c69f1bc9e504";

  outputs = { nixpkgs, nixpkgs-ghc, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      toolchainPkgs = import nixpkgs-ghc { inherit system; };
      haskellPackages = toolchainPkgs.haskell.packages.ghc9103;
      ghc = haskellPackages.ghc;
      cabal = toolchainPkgs.cabal-install;
      sekiPackage = haskellPackages.callCabal2nix "seki" ./. { };
      configCompiler = haskellPackages.ghcWithPackages (p: with p; [
        aeson binary bytestring containers deepseq directory filepath network
        process stm text transformers unix QuickCheck tasty tasty-hunit
        tasty-quickcheck temporary xeno
      ]);
      configArchive = toolchainPkgs.runCommand "seki-config-source" {
        nativeBuildInputs = [ cabal ];
        src = toolchainPkgs.lib.cleanSource ./.;
      } ''
        export HOME="$TMPDIR/home"
        mkdir -p "$HOME" "$out"
        cd "$src"
        cabal sdist --builddir="$TMPDIR/dist" --output-directory="$out"
      '';
      configEnvironment = toolchainPkgs.writeShellApplication {
        name = "seki-config-env";
        runtimeInputs = [
          configCompiler cabal toolchainPkgs.pkg-config toolchainPkgs.stdenv.cc
          toolchainPkgs.python3 toolchainPkgs.coreutils
        ];
        text = ''
          export PKG_CONFIG_PATH="${toolchainPkgs.lib.makeSearchPath "lib/pkgconfig" [
            toolchainPkgs.wayland.dev toolchainPkgs.libxkbcommon.dev
          ]}"
          export GHC_ENVIRONMENT=-
          exec "$@"
        '';
      };
      configLauncher = toolchainPkgs.writeShellScriptBin "seki" ''
        unset SEKI_CONFIG_CABAL_CONFIG
        export SEKI_CONFIG_ARCHIVE=${configArchive}/seki-0.1.0.0.tar.gz
        export SEKI_CONFIG_GHC=${configCompiler}/bin/ghc
        export SEKI_CONFIG_CABAL=${cabal}/bin/cabal
        export SEKI_CONFIG_ENV=${configEnvironment}/bin/seki-config-env
        export SEKI_CONFIG_TEMPLATES=${./examples/config}
        export SEKI_CONFIG_DEFAULT=${sekiPackage}/bin/seki-default
        exec ${sekiPackage}/bin/seki "$@"
      '';
      configDelivery = toolchainPkgs.runCommand "seki-config-delivery" { } ''
        mkdir -p "$out/bin" "$out/share/seki"
        ln -s ${configEnvironment}/bin/seki-config-env "$out/bin/seki-config-env"
        ln -s ${sekiPackage}/bin/seki-default "$out/bin/seki-default"
        ln -s ${configLauncher}/bin/seki "$out/bin/seki"
        ln -s ${configArchive}/seki-0.1.0.0.tar.gz "$out/share/seki/source.tar.gz"
      '';
      fcitx5WithMozc = pkgs.qt6Packages.fcitx5-with-addons.override {
        addons = [ pkgs.fcitx5-mozc ];
      };
      version = package: package.version or "unknown";
    in
    {
      packages.${system}.config-delivery = configDelivery;

      devShells.${system} = {
        unit = haskellPackages.shellFor {
          packages = _: [ sekiPackage ];
          withHoogle = false;
          nativeBuildInputs = [ cabal ];
        };

        ci = haskellPackages.shellFor {
          packages = _: [ sekiPackage ];
          withHoogle = false;
          nativeBuildInputs = [
            cabal
            fcitx5WithMozc
            pkgs.coreutils
            pkgs.dbus
            pkgs.dejavu_fonts
            pkgs.foot
            pkgs.glib
            pkgs.stdenv.cc
            pkgs.pkg-config
            pkgs.pixman
            pkgs.wlroots_0_20
            pkgs.wayland
            pkgs.libxkbcommon
            pkgs.river
            pkgs.waybar
            pkgs.python3
            pkgs.wlr-randr
            pkgs.wtype
            pkgs.xdotool
            pkgs.xterm
            pkgs.xwayland
          ];

          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.dejavu_fonts ];
          };
          WLR_RENDERER = "pixman";
        };
      };

      integrationVersions.${system} = {
        cabal-install = version cabal;
        dbus = version pkgs.dbus;
        fcitx5 = version pkgs.fcitx5;
        fcitx5-mozc = version pkgs.fcitx5-mozc;
        foot = version pkgs.foot;
        ghc = version ghc;
        nixpkgs = "55bfb4d7437bd4e09ab7cba58a43bd68eb310044";
        nixpkgs-ghc = "3ec636362e1816bc2f0888fa0fa2c69f1bc9e504";
        renderer = "pixman";
        river = version pkgs.river;
        waybar = version pkgs.waybar;
        wlr-randr = version pkgs.wlr-randr;
        wtype = version pkgs.wtype;
        xdotool = version pkgs.xdotool;
        xterm = version pkgs.xterm;
        xwayland = version pkgs.xwayland;
      };
    };
}
