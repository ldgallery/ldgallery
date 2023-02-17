# ldgallery - A static generator which turns a collection of tagged
#             pictures into a searchable web gallery.
#
# Copyright (C) 2019-2022  Pacien TRAN-GIRARD
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

{
  description = "A static web gallery generator with tags";

  inputs = {
    # GHC 9.2: https://github.com/NixOS/nixpkgs/pull/202022
    nixpkgs.url = "github:NixOS/nixpkgs/445f264";
    flake-utils.url = "github:numtide/flake-utils";
    flaky-utils.url = "git+https://cgit.pacien.net/libs/flaky-utils";
  };

  outputs = { self, nixpkgs, flake-utils, flaky-utils }:
  flake-utils.lib.eachDefaultSystem (system: let
    pkgs = nixpkgs.legacyPackages.${system};
    ldgalleryVersion = "2.1";
    devTools = with pkgs; [
      # generic
      tmux

      # viewer
      nodejs-16_x
      yarn

      # compiler
      stack
    ];

  in pkgs.lib.fold pkgs.lib.recursiveUpdate { } [
  (rec {
    packages = rec {
      compiler = pkgs.haskell.lib.compose.overrideCabal (super: {
        pname = "ldgallery-compiler";
        version = ldgalleryVersion;

        buildTools = (super.buildTools or []) ++ [ pkgs.makeWrapper ];

        postInstall = ''
          ${super.postInstall or ""}

          # wrapper for runtime dependencies registration
          wrapProgram "$out/bin/ldgallery" \
            --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.imagemagick ]}

          # bash completion
          mkdir -p "$out/share/bash-completion/completions"
          "$out/bin/ldgallery" \
            --help=bash \
            > "$out/share/bash-completion/completions/ldgallery"
        '';
      }) (pkgs.haskellPackages.callCabal2nix "" ./compiler { });

      viewer = pkgs.mkYarnPackage {
        pname = "ldgallery-viewer";
        version = ldgalleryVersion;
        src = ./viewer;

        buildPhase = ''
          # Make the node_module directory writable because ESLint and Webpack
          # want to write in it during the build…
          mv deps/ldgallery-viewer/node_modules{,links}
          mkdir deps/ldgallery-viewer/node_modules
          cp -r deps/ldgallery-viewer/node_modules{links/.bin,}

          export HOME=/build
          yarn --offline run lint
          yarn --offline run build
        '';

        installPhase = ''
          mkdir -p $out/share/ldgallery
          mv deps/ldgallery-viewer/dist $out/share/ldgallery/viewer
        '';

        doDist = false;  # no need to generate a source tarball
      };

      man = pkgs.stdenv.mkDerivation {
        pname = "ldgallery-man";
        version = ldgalleryVersion;
        src = ./.;

        nativeBuildInputs = with pkgs; [ pandoc ];
        installPhase = ''
          mkdir -p $out/share/man/man{1,7}

          pandoc --standalone --to man \
            "compiler/ldgallery.1.md" \
            --output "$out/share/man/man1/ldgallery.1"

          pandoc --standalone --to man \
            "viewer/ldgallery-viewer.7.md" \
            --output "$out/share/man/man7/ldgallery-viewer.7"

          pandoc --standalone --to man \
            "ldgallery-quickstart.7.md" \
            --output "$out/share/man/man7/ldgallery-quickstart.7"
        '';
      };

      # compiler + viewer + man pages bundle
      ldgallery = pkgs.symlinkJoin {
        name = "ldgallery";
        version = ldgalleryVersion;
        paths = [
          man
          (with pkgs.haskell.lib.compose; overrideCabal (super: {
            prePatch = ''
              # add viewer dist to compiler bundled resources
              rm data/readme.md
              ln -s "${viewer}/share/ldgallery/viewer" "data/"
              ${super.prePatch or ""}
            '';
          }) (justStaticExecutables compiler))
        ];
      };

      example = pkgs.stdenv.mkDerivation {
        pname = "ldgallery-example";
        version = ldgalleryVersion;
        src = ./example;
        nativeBuildInputs = [ ldgallery ];
        buildPhase = ''
          # Need UTF-8: https://github.com/ldgallery/ldgallery/issues/341
          export LC_ALL=C.UTF-8
          ldgallery --input-dir src --output-dir $out --with-viewer
        '';
        installPhase = ":";
      };

      default = ldgallery;
    };

    apps = rec {
      ldgallery = flake-utils.lib.mkApp {
        drv = packages.default;
      };

      default = ldgallery;
    };

    devShell = flaky-utils.lib.mkDevShell {
      inherit pkgs;
      tools = devTools;
      shell = null;
    };
  })

  (flaky-utils.lib.mkSandboxSystem {
    inherit pkgs;
    restrictNetwork = false;
    patchQemu9p = true;
    tools = devTools;
    envVars = {
      # File modification watch doesn't work through the VM for live reload.
      VUE_APP_WEBPACK_WATCH_POLL = "1000";
    };
    config = {
      # The viewer's build and devel server are resource-hungry.
      virtualisation.cores = 2;
      virtualisation.memorySize = 2 * 1024;

      virtualisation.forwardPorts = [
        { from = "host"; host.port = 8085; guest.port = 8085; }  # vue-cli
      ];
    };
  })

  ]);
}
