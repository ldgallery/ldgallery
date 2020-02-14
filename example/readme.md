ldgallery example gallery
=========================

This directory contains an example source directory that _ldgallery_ can understand to generate a gallery.


Content
-------

```
./example/out                -- the output directory for the compiled version of the gallery
./example/src                -- source directory with pictures, configuration, tags and metadata
├── _DSC8808-1.jpg           -- a picture
├── _DSC8808-1.jpg.yaml      -- its associated sidecar metadata file
├── Glacier 3000             -- a directory grouping gallery items
│   ├── thumbnail.jpg        -- a thumbnail for the "Glacier 3000" directory
│   ├── directory.yaml       -- sidecar metadata file for the "Glacier 3000" directory
│   ├── _DSC5475.jpg
│   ├── _DSC5475.jpg.yaml
│   ├── _DSC5542.jpg
│   └── _DSC5542.jpg.yaml
├── directory.yaml           -- sidecar metadata file for the root directory
└── gallery.yaml             -- gallery settings file
```
