---
title: "index.json: item tags as bit array"
author: pacien
date: 2022-10-16 (v1)
---


# Abstract

This is a proposal for changing the structure of `index.json` to use bit arrays
for item tags instead of a list of strings.


# Version history

- 2022-10-16: initial proposal

---


# Goals

The main goal of this change is to make filtering items based on the presence
or absence of tags more straightforward and faster, to eliminate the need to
build any type of additional index (such as a hashmap on the consuming side).

This could also reduce the amount of duplicated data in the `index.json` and
reduce its size on disk and in memory. This is not a primary goal, as transport
compression (gzip) and string interning are already doing a good job at
reducing the size on the network and in memory.


# Proposed changes

Currently, the produced `index.json` files containing the gallery item tree is
structured as follows:

```hs
data GalleryIndex = GalleryIndex
  { properties :: ViewerConfig
  , tree :: GalleryItem
  }

data GalleryItem = GalleryItem
  { title :: String
  , datetime :: ZonedTime
  , description :: String
  , tags :: [Tag]
  , path :: Path
  , thumbnail :: Maybe Thumbnail
  , properties :: GalleryItemProps
  }
```

It is proposed to:

1. Add a field `allTags` to `GalleryIndex`, containing an aggregated list of
   the distinct string tags of all the gallery's items.

2. Replace the list of string tags in the `tags` field of `GalleryItem`s with
   a bit array. Each bit maps to a tag in the list `GalleryIndex.allTags`, at
   the index of the bit counting from the least significant bit. Each bit
   indicates the presence (`1`) or absence (`0`) of the corresponding tag on
   the item. This bit array is serialised as an integer in the output JSON
   file.

The new resulting structure would be as follows:

```hs
data GalleryIndex = GalleryIndex
  { properties :: ViewerConfig
  , allTags :: [Tag]                  -- 1. new aggregated list
  , tree :: GalleryItem
  }

data GalleryItem = GalleryItem
  { title :: String
  , datetime :: ZonedTime
  , description :: String
  , tags :: BitArray                  -- 2. now a bit array
  , path :: Path
  , thumbnail :: Maybe Thumbnail
  , properties :: GalleryItemProps
  }
```


# Usage and benefits

## Checking for tags

Testing for the presence or absence of one or multiple tags on items becomes a
simple bitwise mask operation instead of a more costly scan over a list of
strings.

This should make constructing a tag to item mapping unnecessary for consumers
of the gallery index.

For example, in Haskell:

```hs
newtype TagMask = BitArray

tagsMask :: GalleryIndex -> [Tag] -> TagMask
tagsMask (GalleryIndex { allTags }) tags =
  Data.Array.BitArray (0, len allTags) $ map (`elem` tags) allTags

hasAllTags :: GalleryItem -> TagMask -> Bool
hasAllTags (GalleryItem { tags }) wanted =
  (tags `Data.Array.BitArray.and` wanted) == wanted
```

## Retrieving the list of string tags

Rebuilding the list of string tags of an item remains trivial:

```hs
tagsList :: GalleryIndex -> GalleryItem -> [Tag]
tagsList (GalleryIndex { allTags }) (GalleryItem { tags }) =
  GHC.Utils.Misc.filterByList (Data.Array.BitArray.elems tags) allTags
```


# Remarks

## Order of aggregated list of tags

The list of tags `GalleryIndex.allTags` may be sorted in decreasing order of
occurrences of the tags, which may keep the serialised integers shorter
assuming that items having only the most common tags are the most common.

## Large integers for bit arrays

Bit arrays are chosen to be serialised as integers for the sake of compactness
and ease of use through standard integer bitwise operations.

This representation requires as many bits as the total number of distinct tags
used within a gallery.

The JSON format allows arbitrary large integers.

The default number type used by most JavaScript engines however only supports a
maximum of 53 bits. This can be worked around by using the now standard
[`BigInt`][bigint-mdn] type. At the time of writing, this type is [reported to
be widely supported][bigint-caniuse].

[bigint-mdn]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
[bigint-caniuse]: https://caniuse.com/bigint


# Proposed implementation plan

1. Update index file format in compiler and viewer, not touching the rest of
   the code, relying on adapters.

2. Update the viewer querying to filter with bit array operations directly.

