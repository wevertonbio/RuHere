# Relocate a column in a data frame

These functions move one column to a new position in a data frame,
either immediately *after* or *before* another column, while preserving
the order of all remaining columns. They are lightweight base-R
utilities equivalent to
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html),
but without external dependencies.

## Usage

``` r
relocate_after(df, col, after)

relocate_before(df, col, before)
```

## Arguments

- df:

  (data.frame) a data.frame whose columns will be reordered.

- col:

  (character) the name of the column to move.

- after:

  (character) for `relocate_after()`: the column after which `col` will
  be placed.

- before:

  (character) for `relocate_before()`: the column before which `col`
  will be placed.

## Value

A data.frame with columns reordered.
