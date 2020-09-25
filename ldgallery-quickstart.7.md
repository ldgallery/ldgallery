---
pagetitle: Quickstart guide - ldgallery
title: LDGALLERY-QUICKSTART(7) ldgallery
author: Pacien TRAN-GIRARD, Guillaume FOUET
date: 2020-09-19 (v2.0)
---

# ABOUT

This document is a step-by-step guide showing how to create and compile a simple gallery and get familiar with _ldgallery_.

It mainly describes how to structure the source gallery directory accepted by the _ldgallery_ compiler.

```
[source gallery directory with items and their tags]
  |
  | ldgallery compiler
  v
[generated web gallery with embedded web viewer]
  |
  | copy
  v
[web server]
```


# QUICKSTART GUIDE

## Step 1: initialising the gallery source directory

A new gallery can be initialised by creating a directory containing a gallery configuration file named __gallery.yaml__.

```
./monument-gallery-source
└── gallery.yaml ----------- gallery settings file
```

__gallery.yaml__ holds the settings of the gallery.
Its content can be as follows:

```yaml
# gallery.yaml: ldgallery example gallery configuration file.
# See ldgallery(1) for a list of available configuration keys.

galleryTile: Monuments of the World

tagCategories:
  - city
```

## Step 2: adding items

A new item, say a picture file named "DSC0001.jpg", can now be added to the directory created at the previous step.

Optionally, some metadata such as a title and some tags can be associated by creating a file named "DSC0001.jpg.yaml" at the same location.

```
./monument-gallery-source
├── gallery.yaml ----------- gallery settings file
├── DSC0001.jpg ------------ a picture
└── DSC0001.jpg.yaml ------- its associated optional sidecar metadata file
```

The sidecar metadata file "DSC0001.jpg.yaml" can have the following content:

```yaml
# DSC0001.jpg.yaml: ldgallery metadata sidecar file for DSC0001.jpg.
# See ldgallery(1) for a list of available keys.

title: The Eiffel Tower

tags:
  - city:Paris
  - tower
```

## Step 3: compiling the gallery

The gallery can now be compiled by running the following command in a terminal with the right path to the gallery directory created during the previous steps:

```sh
ldgallery \
  --with-viewer \
  --input-dir ./monument-gallery-source \
  --output-dir ./monument-gallery-output
```

If the compiler was installed manually through the extraction of a pre-built archive,
it might be necessary to specify the full path of the installation:

```sh
<installation path>/ldgallery \
  --with-viewer=<installation path>/viewer \
  --input-dir ./monument-gallery-source \
  --output-dir ./monument-gallery-output
```

Running the command above produces a directory named "monument-gallery-output" in the current directory,
which contains the compiled gallery and a web viewer ready to be copied to some web server.

The target web host doesn't need to run any additional software besides a web server correctly configured to serve flat static files.


# TIPS

## Version control

Some standard version-control software such as Git or Mercurial can easily be used to keep track of the evolutions of the gallery directory,
thanks to the text-based format used for the sidecar metadata files.

## Automated compilation and deployment

The gallery can quickly be deployed by using a command such as `rsync`.

The compilation and upload commands can be combined in a Makefile or made part of a script for faster and more convenient deployments.
Such scripted procedure can then further be automated through Continuous Integration hooks.


# SEE ALSO

Related manual pages: __ldgallery__(1), __ldgallery-viewer__(7).

The ldgallery source code is available on <https://ldgallery.pacien.org>.


# LICENSE

Copyright (C) 2019-2020  Pacien TRAN-GIRARD and Guillaume FOUET.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU Affero General Public License for more details <https://www.gnu.org/licenses/agpl-3.0.html>.
