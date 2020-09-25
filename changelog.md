# Changelog

This file lists notable changes that have been made to the application on each release.
Releases are tracked and referred to using git tags.

## v2.0 - 2020-09-25
- Thumbnails are now allowed for all files in addition to directories.
  __Breaking change__: directory thumbnails are now named "\_thumbnail.ext" instead of "\_directory.ext".
- Plain text, PDF, audio and video items are now displayed within the web application in browsers which support those formats.
- Items can now have a timestamp.
  Date and time can be given through the "datetime" key in sidecar metadata files.
  By default, this is set to the last modification date and time of the file itself.
- Items can now have an optional description, given through the option of the same name in sidecar metadata files.
  Rich text formatting is possible through the use of the GitHub-Flavoured Markdown (GFM) syntax.
- An information panel has been added to the viewer.
  It displays the title, date and time, as well as the description associated to the viewed item.
- Items can now be sorted by name and date through a newly introduced sorting menu in the viewer.
  A default order can be configured in the viewer's configuration file with the "initialItemSort" option.
  The default behaviour is to sort items in chronological order.
- Tag suggestions are now limited to the first 10 most used tags for each category.
  The hidden suggestions now have to be expanded by the user.
  This limit can be modified or disabled with the newly introduced "initialTagDisplayLimit" option.
- The viewer can now load alternative configuration files through an optional query parameter.
- The index file to load can now be specified in the viewer configuration file with the "galleryIndex" option.
- Gallery build time has been reduced through more extensive caching (now reusing item metadata from previous compilations).


## v1.0 - 2020-05-02
- First release.
