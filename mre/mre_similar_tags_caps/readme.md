# Minimal Reproducible Example

Having two or more similar tags which differ only in their casing results in
the lowercase variant to be inserted instead of the other ones.

## Reproduction steps

1. Type `Test` in the tag input box and press enter.

## Expected result

The tag `Test` (starting with a capital letter) should be added to the search
query.

## Actual result

The tag `test` (starting with a lower case letter) is instead added to the
search query.

## Version info

Bug present in ldgallery v2.2.

---

Having two or more similar tags which differ only in their casing results in
unrequested insertions in the query.

## Reproduction steps

1. Type `test` in the tag input box and press enter.
2. Press the enter key or click on the "Search" button,
   one or multiple times!

## Expected result

It should just search for items with the `test` tag and display the result.

## Actual result

The tag `Test` is added to the input field even though it was never requested.
Trying to search again results in more instances of `test` and `Test` to be
added to the search field.

No result is found because the resulting "forced" search query is wrong.

## Version info

Bug present in ldgallery v2.2.
