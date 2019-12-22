# ldgallery design notes

_ldgallery_ consists of two main components that are packaged and distributed together, but are otherwise functionally independent:

* a __compiler__ which turns a collection of pictures and sidecar metadata files into their compressed/normalised and aggregated versions respectively, and
* a web __viewer__ in the form of a single-page application, rendering the output of the compiler as a searchable picture gallery.


## Gallery compiler

### Input

#### Directory structure

The compiler takes a source directory as input which shall contain item resource files and associated metadata sidecar files.  Those items may be recursively grouped into multiple levels of sub-directories.

Example source directory structure:

```
example-gallery-source
├── _DSC8808-1.jpg           -- a picture
├── _DSC8808-1.jpg.yaml      -- its associated sidecar metadata file
└── Glacier 3000             -- a directory grouping gallery items
    ├── _DSC5475.jpg
    ├── _DSC5475.jpg.yaml
    ├── _DSC5542.jpg
    └── _DSC5542.jpg.yaml
```


#### Metadata sidecar file

Metadata associated to items are stored in YAML sidecar files of the same name, with the `.yaml` extension appended.  The use of plain text sidecar files allow for easier editing without any special tool in a unified manner for multiple types of files (pictures, videos, ebooks, ...).  The metadata contained within item files are simply ignored.

Tags are given with a group hierarchy separated by `.`, which allows generic searches as well as implicit disambiguation.

Example metadata sidecar file:

```yaml
title: Some title

date: 2019-12-20T16:51:52+00:00

description: >
  Some multiline description
  that may contain some markdown later.

tags:
  - photographer.someone
  - location.france.paris
  - monochromatic
```

Possible evolutions:

* Fallback values may later be defined for each field, making them optional.
* The description field could be allowed to contain markdown-formatted content, which would be rendered by the __viewer__ app.
* Other keys could be added to allow the definition of specific transform/compilation/displaying parameters on a particular file.
* Metadata sidecar files could be added for directories as well in some `index.yaml` file.


#### Gallery configuration file

The gallery YAML configuration file contains the __compiler__'s and the __viewer__'s settings in two different sections.

Proposed configuration file, named `gallery.yaml` at the root of the source directory:

```yaml
compiler:
  # TODO: configuration options to be defined
  # format normalisation?
  # image maximum size?
  # item compression?
  # thumbnail size?
  # thumbnail generation algorithm?

viewer:
  # TODO: configuration options to be defined
  # separately shown tags and their colours
  # use hash in URL (useful for use without webserver url rewrite)?
```


### Output

#### Directory structure

```
data
├── items                  -- original/normalised item directory
│   ├── _DSC8808-1.jpg
│   └── Glacier 3000
│       ├── _DSC5475.jpg
│       └── _DSC5542.jpg
├── thumbnails             -- item thumbnails directory
│   ├── _DSC8808-1.jpg
│   └── Glacier 3000
│       ├── _DSC5475.jpg
│       └── _DSC5542.jpg
├── index.json             -- content index file
└── viewer.json            -- viewer configuration file
```


#### Viewer configuration file

The content of the `viewer` section of the gallery configuration file is used, without any transformation by the __compiler__, to generate the `viewer.json` file located at the root of the output directory.


#### Gallery items index

The __compiler__ generates a global index file `index.json` aggregating the metadata of all the source sidecar files as a tree of items as described below.  Its root node is a directory item.  The type of each property node is marked by its `type` field.

Directory items aggregate their tags from the items below it in order to be displayed in search results.

Resource paths are rooted with respect to the data output directory which serves as the base path.

Serialised item structure:

```json
{
  "_comment": "common fields",

  "title": "Some title",
  "date": "2019-12-20T16:51:52+00:00",
  "description": "Some multiline description\nthat may contain some markdown later.",

  "tags": [
    "photographer.someone",
    "location.france.paris",
    "monochromatic"
  ],

  "path": "[resource path]",
  "thumbnail": "[resource path | null]"},


  "_comment": "type-dependent",

  "properties": {
    "type": "image",
    "filesize": 12345,
    "resolution": { "width": 123, "height": 456 }
  },

  "properties": {
    "type": "video",
    "filesize": 12345
  },

  "properties": {
    "type": "directory",
    "items": [ { "_comment": "item objects..." } ]
  }
}
```


#### Normalised items and thumbnails

Gallery items are normalised and made available in the `items` sub-directory of the data output directory.  Normalisation consists of optional transcoding and other transforms as configured by the user in the compiler part of the `gallery.yaml` configuration file.

Thumbnails are also generated for those items and are placed in the `thumbnails` sub-directory.  Thumbnails of pictures in particular are resized using Lanczos resampling.  Directory thumbnails may later be generated by picking or assembling one or multiple of its items, or defined explicitely by the user.

The structure of the input directory is kept in both those output sub-directories.  Input item files and directories also keep their original names.


## Gallery viewer

The __viewer__ is a single-page web application which displays the gallery's content.

It runs fully on the client's side and remains usable without any back-end.  It renders the compiled resources from the `data` directory, placed at its root, generated by the __compiler__.


### URL anchor

The page anchor is updated and used by the application to reflect the state of its current state in such a way that any view can be shared and loaded back by copying the URL and its anchor.  It should in particular contain the current directory or item, as well as the filtering query.


### Directory/collection/grid view

The application starts by showing a grid view of the root directory of the gallery.

This combined view offers the possiblity to the user to navigate to other items and to search recursively through the current directory.


#### Directory breadcrumbs

The directory view displays the current directory path, with links allowing the user to navigate into parent ones.

```
Gallery / Some directory / Sub-directory
```


#### Filering query

A query field allows the user to search through and filter the items of the current directory and its sibling items recursively.  This search field allows restriction on tags in a hierarchical manner.  It should feature some sort of autocompletion, as well as an adjacent list of tags that are available for filtering.  Positive as well as negative filtering should be possible, allowing the user to exclude some tags from the search results.

For instance, the query `france -chessy` should match an item tagged with `location.france.paris`, but not `location.france.chessy`.

The search is performed on the client side, either by scanning the item tree or with the use on some client-side indexer such as [Elasticlunr].

In addition to tag-based filtering, full-text search on the `title` and `description` could later be added by leveraging such indexer.

[Elasticlunr]: http://elasticlunr.com/


#### Thumbnail grid

The content of a directory are displayed as a grid of thumbnails which allows to navigate through sub-directories and to view items.

By default, the content is rendered in the same ordered as listed in `index.json`.  The user could later be presented with a menu allowing to sort those items by name, date for example.


### Item view

Items other than directories are displayed by this view, making use of most of the screen space to render the element.

This view should as well display the title, description, date, tags and other information associated to the item.  Tags in particular are displayed in a grouped manner as determined in `viewer.json`.

It should be possible to navigate between items of the same directory as the current one through a thumbnail reel and previous/next links.
