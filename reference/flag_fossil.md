# Flag fossil records

This function identifies occurrence records that correspond to fossils,
based on specific search terms found in selected columns.

## Usage

``` r
flag_fossil(
  occ,
  columns = c("basisOfRecord", "occurrenceRemarks"),
  fossil_terms = NULL
)
```

## Arguments

- occ:

  (data.frame) a data frame containing the occurrence records to be
  examined, preferably standardized using
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md).
  Must contain the columns specified in `columns`.

- columns:

  (character) vector of column names in `occ` where the function will
  search for the term `"fossil"` or other fossil-related expressions.
  Default is `c("basisOfRecord", "occurrenceRemarks")`.

- fossil_terms:

  (character) optional vector of additional terms that indicate a fossil
  record (e.g., `"paleontological"`, `"subfossil"`). Default is `NULL`.

## Value

A `data.frame` that is the original `occ` data frame augmented with a
new column named `fossil_flag`. Records identified as fossils receive
`FALSE`, while all other records receive `TRUE`.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Flag fossil records
occ_fossil <- flag_fossil(occ = occurrences)
```
