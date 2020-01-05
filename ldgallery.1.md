% LDGALLERY(1) ldgallery user manual
% Pacien TRAN-GIRARD, Guillaume FOUET
% 2020-01-05 (v0.1.0.0-SNAPSHOT)


# NAME

ldgallery - a static web gallery generator with tags


# DESCRIPTION

ldgallery is a static gallery generator which turns a collection of tagged pictures into a searchable web gallery.

The ldgallery compiler program processes pictures and aggregates metadata from plain text sidecar files to generate an indexed version of the gallery.  It can optionally output a static web viewer along, which allows the content to be presented and searched through from a JavaScript-enabled web browser.  This client-side web application does not require any special software on the server's side.


# COMMAND

ldgallery [\--input-dir=_./_] [\--output-dir=_./out_] [\--with-viewer]

Available options are:

-i, \--input-dir=_DIR_
: Gallery source directory.  Defaults to the current directory.

-o, \--output-dir=_DIR_
: Generated gallery output path.  Defaults to ./out.

-r, \--rebuild-all
: Invalidate cache and recompile everything.

-w, \--with-viewer
: Include the static web viewer in the output.

-h, \--help
: Display help message.

\--version
: Print version information.

\--numeric-version
: Print just the version number.


# INPUT GALLERY STRUCTURE

A gallery source directory contains the gallery items and their sidecar metadata files, optionally grouped inside sub-directories.
Directory thumbnails can be set by placing a picture file named "thumbnail", with any image file extension, inside of it.

An example input gallery directory structure could be as follows:

```
./example-gallery
├── DSC0001.jpg --------- a picture
├── DSC0001.jpg.yaml ---- its associated sidecar metadata file
├── Some directory ------ a directory grouping gallery items
│   ├── thumbnail.jpg --- a thumbnail for its parent directory
│   ├── DSC0002.jpg
│   ├── DSC0002.jpg.yaml
│   ├── DSC0003.jpg
│   └── DSC0003.jpg.yaml
└── gallery.yaml -------- gallery settings file
```


# ITEM METADATA SIDECAR

Item metadata are read from sidecar files of the same name, with the ".yaml" extension appended.
When a sidecar file is absent or a particular key omitted, values are set as empty or to their fallback value specified below.
Metadata contained within item files themselves (e.g. Exif fields for pictures) are ignored.

title
: Title of the item.  Defaults to the name of the file.

date
: ISO 8601-formatted date and time.

description
: Description for the item.

tags
: List of tags for the item.  Tag groups can be defined using prefixes separated by "." (dot).


# GALLERY CONFIGURATION

The gallery settings reside in a file named "gallery.yaml" located at the root of the gallery's source directory.

compiler.galleryName
: Name of the gallery.  Defaults to "Gallery".

compiler.ignoreFiles
: Regular expression matching the name of files to ignore.

compiler.implicitDirectoryTag
: Whether to implicitely tag items with the name of their immediate parent directory. 

compiler.thumbnailMaxResolution.width
: Maximum width in pixels of the item thumbnails, 400 by default.

compiler.thumbnailMaxResolution.height
: Maximum height in pixels of the item thumbnails, 400 by default.

compiler.pictureMaxResolution.width
: Maximum width in pixels of the picture items, unlimited by default.

compiler.pictureMaxResolution.height
: Maximum height in pixels of the picture items, unlimited by default.

viewer.defaultSearchMode [TODO]
: Default search view mode ("highlight" or "filter").  Defaults to "filter".

viewer.defaultSearchQuery [TODO]
: Default search query string.

viewer.defaultSortOrder [TODO]
: Default sort order ("alphanumeric", "reverse-alphanumeric", "date", "reverse-date").  Defaults to "date".

viewer.tagGroups[].tag [TODO]
: Tag prefix defining the tag group.

viewer.tagGroups[].order [TODO]
: Order in which to display tag groups.

viewer.tagGroups[].colour [TODO]
: Colour associated to the tag group.

viewer.hiddenTags [TODO]
: List of tags to hide by default.  Items bearing one of those tags will not be displayed until they are being explicitly searched for.


# SEE ALSO

The ldgallery source code is available on <https://ldgallery.pacien.org>.

Copyright (C) 2019-2020  Pacien TRAN-GIRARD and Guillaume FOUET.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details <https://www.gnu.org/licenses/agpl-3.0.html>.
