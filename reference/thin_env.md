# Flag records that are close to each other in the enviromnetal space

Flags occurrence records for thinning by keeping only one record per
species within the same environmental block/bin.

## Usage

``` r
thin_env(
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  env_layers,
  n_bins = 5,
  prioritary_column = NULL,
  decreasing = TRUE,
  flag_for_NA = FALSE
)
```

## Arguments

- occ:

  (data.frame or data.table) a data frame containing the occurrence
  records. Must contain columns for species, longitude, and latitude.

- species:

  (character) the name of the column in `occ` that contains the species
  scientific names. Default is `"species"`.

- long:

  (character) the name of the column in `occ` that contains the
  longitude values. Default is `"decimalLongitude"`.

- lat:

  (character) the name of the column in `occ` that contains the latitude
  values. Default is `"decimalLatitude"`.

- env_layers:

  (SpatRaster) object containing environmental variables.

- n_bins:

  (numeric) number of bins into which each environmental variable will
  be divided.

- prioritary_column:

  (character) name of a numeric columns in `occ`to define retention
  priority (e.g., quality score, year). See details.

- decreasing:

  (logical) whether to sort records in decreasing order using the
  `prioritary_column` (e.g., from most recent to oldest when the
  variable is `"year"`). Only applicable when `prioritary_column` is not
  `NULL`. Default is `TRUE`.

- flag_for_NA:

  (logical) whether to treat records falling in `NA` cells of
  `env_layers` as valid (`TRUE`) or invalid (`FALSE`). Default is
  `FALSE`.

## Value

The original `occ` data frame with two additional columns:

- `thin_env_flag`: logical indicating whether each record is retained
  (`TRUE`) or flagged as redundant (`FALSE`).

- `bin`: environmental bin ID assigned to each record. Each component of
  the ID corresponds to the bin of one environmental variable.

## Details

This function used
[`get_env_bins()`](https://wevertonbio.github.io/RuHere/reference/get_env_bins.md)
to create a multidimensional grid in environmental space by splitting
each environmental variable into `n_bins` equally sized intervals.
Records falling into the same environmental bin are considered
redundant; only one is kept (based on retention priority when provided),
and the remaining records are flagged.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Flag records that are close to each other in the enviromnetal space
occ_env_thin <- thin_env(occ = occ, env_layers = r)
# Number of flagged (redundant) records
sum(!occ_env_thin$thin_env_flag) #Number of flagged records
#> [1] 1387
```
