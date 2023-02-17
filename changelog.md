# Changelog

This file lists notable changes that have been made to the application on each
release. Releases are tracked and referred to using git tags.


## [next release]

- _None yet._


## v2.2 - 2023-02-17
- New features:
  - viewer: add `CTRL-K` keyboard shortcut for quick search.
  - viewer: added a button to download the current item.
- Bug fixes:
  - compiler: fix detection of dimensions of EXIF-rotated pictures.
    Rebuild the gallery with `--rebuild-all` to purge erroneous cached data.
  - viewer: fix theme quirks (line spacing, icon colours).
  - viewer: fix ghost keyboard hints when the search panel is closed.


## v2.1 - 2022-09-04
- New features:
  - Add support for Markdown-formatted files, which are now rendered.
  - Add support for user-defined splash screen in the viewer.
- Improvements:
  - Item descriptions now support Markdown formatting.
  - `.webp` files are now registered and displayed as pictures.
  - Better focus management (tab and scroll) in the viewer.
  - Prevent flashing during viewer loading.
  - Made the viewer lighter and faster to load.
  - Directories now stay on top of item lists in viewer.
  - Zooming on pictures is now smoother (proportional).
- Bug fixes:
  - Fix cache thumbnail masking in index in the compiler.
  - Fix momentum/kinetic scroll on Safari and Firefox on iOS.
  - Fix sort order button hitbox.
  - Fix viewer component not resetting on item change.
  - Fix "Object is possibly 'null'" error in templates.
  - Fix picture loading failure in edge case.
  - Fix scroll position issue when switching screen orientation.
  - Fix item aspect-ratio when the search panel is open.
- Miscellaneous:
  - The project is now available as a Nix Flake.
  - Framework upgrade: Vue 2 to Vue 3
  - Replaced viewer components for dropdown, toast, button, tag, input, loader.
  - The Windows bundle now includes ImageMagick with HDRI enabled.
  - Portable builds now look for the viewer at its runtime location.
  - Plain text files are now displayed inline instead of using an iframe.


## v2.0 - 2020-09-25
- Thumbnails are now allowed for all files in addition to directories.
  __Breaking change__: directory thumbnails are now named "\_thumbnail.ext"
  instead of "\_directory.ext".
- Plain text, PDF, audio and video items are now displayed within the web
  application in browsers which support those formats.
- Items can now have a timestamp. Date and time can be given through the
  "datetime" key in sidecar metadata files. By default, this is set to the last
  modification date and time of the file itself.
- Items can now have an optional description, given through the option of the
  same name in sidecar metadata files. Rich text formatting is possible through
  the use of the GitHub-Flavoured Markdown (GFM) syntax.
- An information panel has been added to the viewer. It displays the title,
  date and time, as well as the description associated to the viewed item.
- Items can now be sorted by name and date through a newly introduced sorting
  menu in the viewer. A default order can be configured in the viewer's
  configuration file with the "initialItemSort" option. The default behaviour
  is to sort items in chronological order.
- Tag suggestions are now limited to the first 10 most used tags for each
  category. The hidden suggestions now have to be expanded by the user. This
  limit can be modified or disabled with the newly introduced
  "initialTagDisplayLimit" option.
- The viewer can now load alternative configuration files through an optional
  query parameter.
- The index file to load can now be specified in the viewer configuration file
  with the "galleryIndex" option.
- Gallery build time has been reduced through more extensive caching (now
  reusing item metadata from previous compilations).


## v1.0 - 2020-05-02
- First release.

