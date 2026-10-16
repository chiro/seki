{
  description = "Pinned seki CI and real-river integration environment";

  inputs.nixpkgs.url =
    "github:NixOS/nixpkgs/55bfb4d7437bd4e09ab7cba58a43bd68eb310044";
  inputs.nixpkgs-ghc.url =
    "github:NixOS/nixpkgs/50ab793786d9de88ee30ec4e4c24fb4236fc2674";

  outputs = { nixpkgs, nixpkgs-ghc, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      toolchainPkgs = import nixpkgs-ghc { inherit system; };
      haskellPackages = toolchainPkgs.haskell.packages.ghc966;
      ghc = haskellPackages.ghc;
      cabal = toolchainPkgs.cabal-install;
      sekiPackage = haskellPackages.callCabal2nix "seki" ./. { };
      fcitx5WithMozc = pkgs.qt6Packages.fcitx5-with-addons.override {
        addons = [ pkgs.fcitx5-mozc ];
      };
      version = package: package.version or "unknown";
    in
    {
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
            pkgs.river
            pkgs.wlr-randr
            pkgs.wtype
            pkgs.xdotool
            pkgs.xterm
            pkgs.xwayland
          ];

          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.dejavu_fonts ];
          };
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
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
        nixpkgs-ghc = "50ab793786d9de88ee30ec4e4c24fb4236fc2674";
        renderer = "pixman";
        river = version pkgs.river;
        wlr-randr = version pkgs.wlr-randr;
        wtype = version pkgs.wtype;
        xdotool = version pkgs.xdotool;
        xterm = version pkgs.xterm;
        xwayland = version pkgs.xwayland;
      };
    };
}
