# Flag records outside a year range

This function identifies occurrence records collected before or after
user-specified years.

## Usage

``` r
flag_year(
  occ,
  year_column = "year",
  lower_limit = NULL,
  upper_limit = NULL,
  flag_NA = FALSE
)
```

## Arguments

- occ:

  (data.frame) a dataset with occurrence records, preferably
  standardized using
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md).
  Must contain the column specified in `year_column`.

- year_column:

  (character) name of the column containing the year in which the
  occurrence was recorded. This column must be numeric.

- lower_limit:

  (numeric) the minimum acceptable year. Records collected before this
  value will be flagged. Default is `NULL`.

- upper_limit:

  (numeric) the maximum acceptable year. Records collected after this
  value will be flagged. Default is `NULL`.

- flag_NA:

  (character) whether to flag records with missing year information.
  Default is `FALSE`.

## Value

A data.frame identical to `occ` but with an additional column named
`year_flag`. Records collected outside the year range specified are
assigned `FALSE`.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Flag records collected before 1980 and after 2010
occ_year <- flag_year(occ = occurrences, lower_limit = 1980,
                      upper_limit = 2010)
```
