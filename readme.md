ldgallery
=========

A static gallery generator which turns a collection of tagged pictures into a searchable web gallery.

The complete list of features, the user manual, demo and download links can be found on the project's website: https://ldgallery.pacien.org.


Build
-----

* Compile the web _viewer_ as detailed in `./viewer/readme.md`.
* Copy/link the compiled _viewer_ to the _ldgallery compiler_ data directory.
* Compile the _ldgallery compiler_ as detailed in `./compiler/readme.md`.


The man page can be generated using [Pandoc]:

```
pandoc --standalone --to man ldgallery.1.md --output=ldgallery.1
```

[Pandoc]: https://pandoc.org/


License
-------

Copyright (C) 2019-2020  Pacien TRAN-GIRARD and Guillaume FOUET.

_ldgallery_ is distributed under the terms of the GNU Affero General Public License v3.0, as detailed in `license.md`.
