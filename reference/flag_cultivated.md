# Flag occurrence records of cultived individuals

This function identifies records of cultivated individuals based on
record description.

## Usage

``` r
flag_cultivated(
  occ,
  columns = c("occurrenceRemarks", "habitat", "locality"),
  cultivated_terms = NULL,
  not_cultivated_terms = NULL
)
```

## Arguments

- occ:

  (data.frame) a data frame containing the occurrence records to be
  examined, preferably standardized using
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md).
  Must contain the columns specified in `columns`.

- columns:

  columns (character) vector of column names in `occ` where the function
  will search for cultivated-related expressions. Default is
  `c("occurrenceRemarks", "habitat", "locality")`.

- cultivated_terms:

  (character) optional vector of additional terms that indicate a
  cultivated individual. Default is NULL, meaning it will use the
  cultivated-related expressions available in
  `RuHere::cultivated$cultivated`.

- not_cultivated_terms:

  (character) optional vector of additional terms that indicate a
  non-cultivated individual. Default is NULL, meaning it will use the
  non cultivated-related expressions available in
  `RuHere::cultivated$not_cultivated`.

## Value

A `data.frame` that is the original `occ` data frame augmented with a
new column named `cultivated_flag`. Records identified as cultivated
receive `FALSE`, while all other records receive `TRUE`.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Flag fossil records
occ_cultivated <- flag_cultivated(occ = occurrences)
```
