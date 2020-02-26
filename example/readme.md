ldgallery example gallery
=========================

This directory contains an example source directory that _ldgallery_ can understand to generate a gallery.


Content
-------

```
./example/out ----------- the output directory for the compiled version of the gallery
./example/src ----------- source directory with pictures, configuration, tags and metadata
├── DSC0001.jpg --------- a picture
├── DSC0001.jpg.yaml ---- its associated sidecar metadata file
├── Ormont-Dessus ------- a directory grouping gallery items
│   ├── _directory.jpg -- a thumbnail for its parent directory
│   ├── _directory.yaml - directory sidecar metadata file
│   ├── DSC0002.jpg
│   ├── DSC0002.jpg.yaml
│   ├── DSC0003.jpg
│   └── DSC0003.jpg.yaml
└── gallery.yaml -------- gallery settings file
```
