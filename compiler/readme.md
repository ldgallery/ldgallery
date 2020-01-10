# ldgallery-compiler


## Build

Building the _ldgallery compiler_ requires the [stack] tool.

[stack]: https://haskellstack.org/

Within the project's directory, use

* `stack setup` to setup the development environment and compiler.
* `stack build` to compile the project.
* or `stack build --fast --file-watch` to automatically compile on file change.
* `stack exec ldgallery-compiler-exe -- --help` to run the compiled program (and displaying its help text for instance).


### Embedded viewer

In order to allow the `ldgallery` command line tool to generate a full gallery which includes the _viewer_, a compiled version of the web app must be placed under `./data/viewer`.  The `--with-viewer` flag will otherwise not be functional.
