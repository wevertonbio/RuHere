# Flag duplicated records

This function identifies duplicated records based on species name and
coordinates, as well as user-defined additional columns or raster cells.
Among duplicated records, the function keeps only one unflagged record,
chosen according to a continuous variable (e.g., keeping the most
recent), a categorical variable (e.g., prioritizing a specific data
source), or randomly.

## Usage

``` r
flag_duplicates(
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  additional_groups = NULL,
  continuous_variable = NULL,
  decreasing = TRUE,
  categorical_variable = NULL,
  priority_categories = NULL,
  by_cell = FALSE,
  raster_variable = NULL
)
```

## Arguments

- occ:

  (data.frame) a data frame containing the occurrence records to be
  examined, preferably standardized using
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md).
  Must contain the columns specified in `species`, `long` and `lat`
  arguments.

- species:

  (character) the name of the column containing species names. Default
  is "species".

- long:

  (character) the name of the column containing longitude values.
  Default is `"decimalLongitude"`.

- lat:

  (character) the name of the column containing latitude values. Default
  is `"decimalLatitude"`.

- additional_groups:

  (character) optional vector of additional column names to consider
  when identifying duplicates. For example, if `"year"` is included,
  records with the same coordinates but different collection years will
  not be flagged. Default is `NULL`.

- continuous_variable:

  (character) optional name of a numeric column used to sort duplicated
  records and select one to remain unflagged. Default is `NULL`, meaning
  that no sorting will occur and the unflagged record will be selected
  randomly.

- decreasing:

  (logical) whether to sort records in decreasing order using the
  `continuous_variable` (e.g., from most recent to oldest when the
  variable is `"year"`). Only applicable when `continuous_variable` is
  not `NULL`. Default is `TRUE`.

- categorical_variable:

  (character) (character) optional name of a categorical column used to
  sort duplicated records and select one to remain unflagged. If
  provided, the order of priority must be specified through
  `priority_categories`. Default is `NULL`.

- priority_categories:

  (character) vector of categories, in the desired order of priority,
  present in the column specified in `categorical_variable`. Only
  applicable when `categorical_variable` is not `NULL`. Default is
  `NULL`.

- by_cell:

  (logical) whether to use raster cells instead of raw coordinates to
  identify duplicates (i.e., all records inside the same raster cell are
  treated as duplicates). If `TRUE`, a `SpatRaster` must be supplied in
  `raster_variable`. Default is `FALSE`.

- raster_variable:

  (SpatRaster) a `SpatRaster` used to identify duplicated records by
  raster cell. Only applicable when `by_cell` is `TRUE`. Default is
  `NULL`.

## Value

A `data.frame` that is the original `occ` data frame augmented with a
new column named `duplicated_flag`. Records identified as duplicated
receive `FALSE`, while all unique retained records receive `TRUE`.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Duplicate some records as example
occurrences <- rbind(occurrences[1:1000, ], occurrences[1:100,])
# Flag duplicates
occ_dup <- flag_duplicates(occ = occurrences)
sum(!occ_dup$duplicated_flag) #Number of duplicated records
#> [1] 100
```
