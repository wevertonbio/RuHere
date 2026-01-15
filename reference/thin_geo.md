# Flag records that are close to each other in the geographic space

Marks occurrence records for thinning by keeping only one record per
species within a radius of 'd' kilometers.

## Usage

``` r
thin_geo(
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  d,
  prioritary_column = NULL,
  decreasing = TRUE,
  remove_invalid = TRUE,
  optimize_memory = FALSE,
  verbose = TRUE
)
```

## Arguments

- occ:

  (data.frame or data.table) a data frame containing the occurrence
  records to be flagged. Must contain columns for species, longitude,
  and latitude.

- species:

  (character) the name of the column in `occ` that contains the species
  scientific names. Default is `"species"`.

- long:

  (character) the name of the column in `occ` that contains the
  longitude values. Default is `"decimalLongitude"`.

- lat:

  (character) the name of the column in `occ` that contains the latitude
  values. Default is `"decimalLatitude"`.

- d:

  (numeric) thinning distance in **kilometers** (e.g., 10 for 10km).

- prioritary_column:

  (character) name of a numeric columns in `occ`to define retention
  priority (e.g., quality score, year). See details.

- decreasing:

  (logical) whether to sort records in decreasing order using the
  `prioritary_column` (e.g., from most recent to oldest when the
  variable is `"year"`). Only applicable when `prioritary_column` is not
  `NULL`. Default is `TRUE`.

- remove_invalid:

  (logical) whether to remove invalid coordinates. Default is `TRUE`.

- optimize_memory:

  (logical) whether to compute the distance matrix using a C++
  implementation that reduces memory usage at the cost of increased
  computation time. Recommended for large datasets (\> 10,000 records).
  Default is FALSE.

- verbose:

  (logical) whether to display messages during function execution. Set
  to TRUE to enable display, or FALSE to run silently. Default is TRUE.

## Value

The original `occ` data frame augmented with a new logical column named
`thin_geo_flag`. Records that are retained after thinning receive
`TRUE`, while records identified as too close to a higher-priority
record receive `FALSE`.

## Details

This function is similar to the `thin()` function from the **spThin**
package, but with an important difference: it allows specifying a
priority order for retaining records.

When a thinning distance is provided (e.g., 10 km), the function
identifies clusters of records within this distance. Within each
cluster, it keeps the record with the highest priority according to the
column defined in `prioritary_column` (for example, keeping the most
recent record if `prioritary_column = "year"`), and flags the remaining
nearby records for removal.

If `prioritary_column` is `NULL`, the priority follows the original
order of rows in the input `occ` data.frame.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Subset occurrences for Araucaria angustifolia
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Thin records using a 10 km distance threshold
occ_thin <- thin_geo(occ = occ, d = 10)
#> Removing 1 invalid records
sum(!occ_thin$thin_geo_flag)  # Number of records flagged for removal
#> [1] 235
# Prioritizing more recent records within each cluster
occ_thin_recent <- thin_geo(occ = occ, d = 10, prioritary_column = "year")
#> Removing 1 invalid records
sum(!occ_thin_recent$thin_geo_flag)  # Number of records flagged for removal
#> [1] 235
```
