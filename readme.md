ldgallery
=========

A static gallery generator which turns a collection of tagged pictures and
media into a searchable web gallery.

The complete list of features, the user manual, demo and download links can be
found on the project's website: <https://ldgallery.pacien.org>.


Usage and build (using the Nix Flake)
-------------------------------------

This program is available as a Nix Flake allowing to build both the viewer
and compiler components and assemble those automatically.

The following commands are available on NixOS, or a Linux or MacOS system
having the Nix package manager installed:

* Just running the program (compiler with bundled viewer):
  * Using the latest release: `nix run github:ldgallery/ldgallery -- --help`
  * Using a local source checkout: `nix run .# -- --help`

* Building individual components locally:
  `nix build .#{compiler,viewer,man} --print-build-logs`


Manual build
------------

Without using the Nix Flake, the project can be built as follows:

* Compile the web _viewer_ as detailed in `./viewer/readme.md`.
* Copy/link the compiled _viewer_ to the _ldgallery compiler_ data directory.
* Compile the _ldgallery compiler_ as detailed in `./compiler/readme.md`.


The man pages can be generated using [Pandoc]:

```
pandoc --standalone --to man ldgallery-quickstart.7.md --output ldgallery-quickstart.7
pandoc --standalone --to man compiler/ldgallery.1.md --output ldgallery.1
pandoc --standalone --to man viewer/ldgallery-viewer.7.md --output ldgallery-viewer.7
```

[Pandoc]: https://pandoc.org/


License
-------

Copyright (C) 2019-2020  Pacien TRAN-GIRARD and Guillaume FOUET.

_ldgallery_ is distributed under the terms of the GNU Affero General Public License v3.0, as detailed in `license.md`.

Builds of this software embed and make use of the following libraries:

* Compiler (Hackage packages)
  * base, licensed under the BSD-3-Clause License
  * containers, licensed under the BSD-3-Clause License
  * data-ordlist, licensed under the BSD-3-Clause License
  * filepath, licensed under the BSD-3-Clause License
  * directory, licensed under the BSD-3-Clause License
  * text, licensed under the BSD-2-Clause License
  * aeson, licensed under the BSD-3-Clause License
  * yaml, licensed under the BSD-3-Clause License
  * cmdargs, licensed under the BSD-3-Clause License
  * parallel-io, licensed under the BSD-3-Clause License
  * Glob, licensed under the BSD-3-Clause License
  * safe, licensed under the BSD-3-Clause License
  * time, licensed under the BSD-3-Clause License
  * process, licensed under the BSD-3-Clause License
* Viewer (npm packages)
  * fortawesome/fontawesome-svg-core, licensed under the MIT License
  * fortawesome/free-solid-svg-icons, licensed under the CC-BY-4.0 and MIT Licenses
  * fortawesome/free-regular-svg-icons, licensed under the CC-BY-4.0 and MIT Licenses
  * fortawesome/vue-fontawesome, licensed under the MIT License
  * buefy, licensed under the MIT License
  * hammerjs, licensed under the MIT License
  * marked, licensed under the MIT License
  * v-lazy-image, licensed under the MIT License
  * vue, licensed under the MIT License
  * vue-class-component, licensed under the MIT License
  * vue-dragscroll, licensed under the MIT License
  * vue-i18n, licensed under the MIT License
  * vue-property-decorator, licensed under the MIT License
  * vue-router, licensed under the MIT License
  * vuex, licensed under the MIT License
  * vuex-class-component, licensed under the ISC License
