---
pagetitle: User manual - ldgallery
title: LDGALLERY(1) ldgallery user manual
author: Pacien TRAN-GIRARD, Guillaume FOUET
date: 2020-02-15 (v0.1.0.0-SNAPSHOT)
---


# NAME

ldgallery - a static web gallery generator with tags


# DESCRIPTION

ldgallery is a static gallery generator which turns a collection of tagged pictures into a searchable web gallery.

The ldgallery compiler program processes pictures and aggregates metadata from plain text sidecar files to generate an indexed version of the gallery.  It can optionally output a static web viewer along, which allows the content to be presented and searched through from a JavaScript-enabled web browser.  This client-side web application does not require any special software on the server's side.


# COMMAND

ldgallery [\--input-dir _./_] [\--output-dir _./out_] [\--with-viewer]

Available options are:

-i, \--input-dir _DIR_
: Gallery source directory.
  Defaults to the current directory.

-o, \--output-dir _DIR_
: Generated gallery output path.
  Must be distinct from the source directory.
  Defaults to ./out.

-x, \--output-index _FILE_
: Generated gallery index output path.
  Defaults to \<output-dir\>/index.json.

-g, \--gallery-config _FILE_
: Gallery configuration file.
  Defaults to \<input-dir\>/gallery.yaml.

-r, \--rebuild-all
: Invalidate cache and recompile everything.

-c, \--clean-output
: Remove unnecessary files from the output directory.

-w, \--with-viewer
: Include the static web viewer in the output.
  The compiled gallery itself is then placed in <\output-dir\>/gallery.

-h, \--help
: Display help message.

\--version
: Print version information.

\--numeric-version
: Print just the version number.


# INPUT GALLERY STRUCTURE

A gallery source directory contains the gallery items and their sidecar metadata files, optionally grouped inside sub-directories.

Directory thumbnails can be set by placing a picture file named "_directory", with any image file extension, inside of directories.

An example input gallery directory structure could be as follows:

```
./example-gallery
├── DSC0001.jpg --------- a picture
├── DSC0001.jpg.yaml ---- its associated sidecar metadata file
├── Some directory ------ a directory grouping gallery items
│   ├── _directory.jpg -- a thumbnail for its parent directory
│   ├── _directory.yaml - directory sidecar metadata file
│   ├── DSC0002.jpg
│   ├── DSC0002.jpg.yaml
│   ├── DSC0003.jpg
│   └── DSC0003.jpg.yaml
└── gallery.yaml -------- gallery settings file
```


# METADATA SIDECAR

File metadata are read from sidecar files of the same name, with the ".yaml" extension appended.
Metadata contained within item files themselves (e.g. Exif fields for pictures) are ignored.

Directory metadata are read from sidecar files named "_directory.yaml" located within the directory.

When a sidecar file is absent or a particular key omitted, values are set as empty or to their fallback value specified below.

title
: Title of the item.
  Defaults to the name of the file or directory.

datetime
: ISO 8601 zoned date and time.
  Defaults to the last modification time of the file itself,
  or the most recent modification date of a directory's items.

description
: Description for the item.

tags
: List of tags for the item.
  Tag groups can be defined using prefixes separated by "." (dot).
  Tags specified in a directory metadata sidecar are applied to all items within that directory.


# GALLERY CONFIGURATION

The gallery settings reside in a file named "gallery.yaml" located at the root of the gallery's source directory.

galleryTitle
: Title of the gallery.  Defaults to "ldgallery".

includedDirectories[]
: Glob patterns of directory names to include in the gallery.  Defaults to ["*"] (matches all directory names).

excludedDirectories[]
: Glob patterns of directory names to exclude from the gallery.  Defaults to [] (none).

includedFiles[]
: Glob patterns of file names to include in the gallery.  Defaults to ["*"] (matches all file names).

excludedFiles[]
: Glob patterns of file names to exclude from the gallery.  Defaults to [] (none).

tagsFromDirectories.fromParents
: Automatically generate tags from the name of parent directories,
  looking up in the hierarchy as far as indicated by this parameter.
  Defaults to 0 (do not generate tags from parent directories).

tagsFromDirectories.prefix
: Prefix to use for tags automatically generated from the parent directories' names.

thumbnailMaxResolution.width
: Maximum width in pixels of the item thumbnails, 400 by default.

thumbnailMaxResolution.height
: Maximum height in pixels of the item thumbnails, 300 by default.

pictureMaxResolution.width
: Maximum width in pixels of the picture items, unlimited by default.

pictureMaxResolution.height
: Maximum height in pixels of the picture items, unlimited by default.


# SEE ALSO

The ldgallery source code is available on <https://ldgallery.pacien.org>.

Copyright (C) 2019-2020  Pacien TRAN-GIRARD and Guillaume FOUET.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Affero General Public License for more details <https://www.gnu.org/licenses/agpl-3.0.html>.
