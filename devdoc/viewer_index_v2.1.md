---
title: "Viewer: index v2.1"
author: pacien
date: 2022-10-25 (v3)
---

# Abstract

This is a short documentation of the viewer's internal index dating from the
first version of this software and still currently in use in version 2.1.

This index was written without proper specifications or documentation, and has
evolved a lot since then.

This document is an attempt at describing how this index is currently being
generated and used, before further work can be done on some proper
specification to clarify the problematic behaviours.


# Document version history

1. 2022-10-15 by pacien: call notes
2. 2022-10-22 by pacien: rewritten
3. 2022-10-25 by pacien: include feedbacks from zeroinformatique


# Description of the current implementation

## Generating the index(es) from the item tree

The gallery item index (`index.json`) is loaded by the `galleryStore` and made
available as the `galleryIndex` attribute.

Two indexes are derived from that file through the `indexFactory` service:

- `tagsIndex`:
  - Maps each tag "part" (component) to items tagged with it.
  - Maps tags to their immediately preceding disambiguating component as child.
  - Includes a normalised version of the tag for searching.
  - Stores the `childPart`, used to generate tag categories indexes.
  - (Stores the left-most `rootPart` component. This is not used anywhere).

- `tagsCategories`:
  - Same as `tagsIndex`, but partitioned by tag category prefixes as defined in
    the viewer configuration.


## Search query input auto-completion "suggestions"

The `TagInput` component suggests tags as the user types in that input field.

This is provided by the `indexFactory.searchTags` function in fuzzy
(non-strict) mode, which only uses the global tag index.

The fuzzy search is implemented as a normalised (lowercase) infix word lookup,
including the disambiguating tag parts.

The auto-completion suggestions are independent of the current directory.
Suggestions yielding no result (incompatible with the current search query) are
not excluded either. The current implementation makes the choice of suggesting
everything because the current index does not allow finer filtering, and
because walking the whole item tree may lead to performance issues.


## Item search

The search query is stored in the URL query. This allows a search to be shared
by copying the current URL.

This URL search query is updated by `LayoutLeft.vue` to match modifications
made through the tag input or related filters "propositions". This component
also updates the store to match the URL query through `galleryStore.search`.

A search query consists of three sets of tags: an intersection list, a forced
inclusion (union) list, and a forced exclusion list. The last two are denoted
with a `+` and `-` modifier prefix before the tag name. The order of the terms
does not matter.

The result computed in `indexSearch.indexSearch` is given by
`(⋂(intersection) ∪ ⋃(forced inclusion)) ∖ ⋃(forced exclusion)`.

The string representation of a query is parsed in `indexFactory`. It is
serialised by taking the `filter.display` property of filters, in
`LayouLeft.vue` for being displayed in the tag input and in the URL.


## Related filters "propositions"

The left pane of the user interface lists related filters "propositions",
related to the current search results or directory being viewed.

Tags in that pane are grouped according to the `tagCategories` gallery
configuration key. (This is currently buggy: some tags can appear in the wrong
category under some circumstances).

The related tags are filtered with respect to the current search query or
directory: only tags that are present on the listed items are shown.

Each "proposed" tag has an occurrence count of the items having that tag in the
whole gallery. (This is inconsistent with the locality of the filter).

This is computed using a full gallery search through the `galleryStore` using
`indexFactory.searchTags` in strict (non-fuzzy) mode.


# Identified issues and proposals

## Issues affecting the end users

- Tags categories and disambiguation aren't properly defined:
  - It is not clear whether intermediate tag components should be treated as
    tags and suggested at all. (They currently are).

- Tags with indirect disambiguations are not handled correctly:
  - Example in `a:b:c`:
    - `b` is a child of `a`, `c` is a child of `b`.
    - But `c` is not registered as a child of `a` in `tagsIndex`.

- Homonymous disambiguated tags are not handled in separate categories.
  - Example with `a` and `b:a`:
    - `a` seems to be shown under category `b`.
  - This seems to be the cause of tags being displayed in the wrong category in
    the suggestion pane.

- The tag input's auto-completion suggests impossible intersections:
  - The fuzzy (non-strict) search does not work the same way as the suggestions
    panel, which restricts the suggestions.
    - This might however be problematic for forced inclusions (union) tags
      which are still meaningful.
    - They could still be listed but greyed for example.

- The tag occurrence counts in the related tags "propositions" pane is
  misleading:
  - This view suggests only the tags for the current search results
    (descendants of the current directory and matching the current search query
    if any),
  - But the occurrence count for each tag is global (on the whole gallery
    instead of the current search results).


## Issues affecting only the developers

- Ambiguous terminology:
  - For example "index" vs "index.json", or "tag suggestions" vs
    "tag propositions" vs "tag completion".
  - This confusion is reflected in the component naming and coupling…
  - A glossary and would help.
  - Refactoring and renaming the modules would help.

- Tight coupling of the tag-related and index operations:
  - It goes all over the place.
  - Some concerns should and can clearly be separated for example:
    - For example query parsing, compiling and actual run on the item tree.
  - The new modules should make use of composition with the rest of the
    components.

- Lack of unit tests:
  - Coupling is preventing easy unit testing.
  - Once the concerns are separated:
    - We'll have clear expected outputs with respect to some input.
    - It should be easier to do unit testing:
      - (perhaps through randomised property testing).

- Minor: relatively verbose and intertwined imperative code:
  - The query parsing and recursive tree operations would probably be more
    elegant in PureScript than Javascript/Typescript.
  - Same with unit and property tests.
