# Minimal Reproducible Example

Multiple issues are shown with this example.

---

Incorrect related filters:

Suggestions for related filters should only contain categories used in the
items in the search result, not all categories happening to contain tags in
the results which have the same name.

## Reproduction steps

1. Search for `animal`.

## Expected result

The related filters suggestions should only suggest `animal:bat`, but not
`object:bat`, since the only result does not have `object:bat`.

## Actual result

Both `animal:bat` and `object:bat` are suggested as related filters.
The latter is incorrect.

## Version info

Bug present in ldgallery v2.2.

---

Incorrect related filters:

Excluding a category should result in all tags in that category being excluded
from the related filters suggestions, even if they happen to contain a tag with
the same name.

## Reproduction steps

1. Search for `-animal`.

## Expected result

The `animal` category should not show up in the related filters suggestions.

## Actual result

The `animal` category still shows up in addition to the `object` category.

## Version info

Bug present in ldgallery v2.2.

---

Incorrect search results and related filters:

Excluding a category should only exclude tags in that category.
It should not exclude items having a tag with the same name in other
categories.

## Reproduction steps

1. Search for `-animal`.

## Expected result

`baseball_bat.md` and `cricket_bat.md` should appear in the search results,
because they do not have any tag in the category `animal`.

## Actual result

The search yields no result at all.
Strangely enough, the tag `bat` is still being suggested in both categories
in the related filters.

## Version info

Bug present in ldgallery v2.2.

---

Incorrect search results:

Excluding a tag within a category should only exclude tags in that category.
It should not exclude items having a tag with the same name in other
categories.

## Reproduction steps

1. Search for `-animal:bat`.

## Expected result

`baseball_bat.md` and `cricket_bat.md` should appear in the search results,
because they do not have that tag in the category `animal`, but `object`.

## Actual result

The search yields no result at all.

## Version info

Bug present in ldgallery v2.2.
