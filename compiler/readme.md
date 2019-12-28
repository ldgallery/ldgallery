# ldgallery-compiler

## Build

Building this project requires the [stack] tool.

[stack]: https://haskellstack.org/

Within the project's directory, use

* `stack setup` to setup the development environment and compiler.
* `stack build` to compile the project.
* or `stack build --fast --file-watch --pedantic` to automatically compile on file change.
* `stack exec ldgallery-compiler-exe -- --help` to run the compiled program (and displaying its help text for instance).
