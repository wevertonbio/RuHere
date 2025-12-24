# Identify Environmental Blocks and Group Nearby Records in Environmental Space

This function creates a multidimensional grid in environmental space by
splitting each environmental variable into `n_bins` equally sized
intervals. It then assigns each occurrence record to an environmental
block (bin combination) and identifies records that fall into the same
block (i.e., records that are close to each other in environmental
space).

The results can be visualized using the
[`plot_env_bins()`](https://wevertonbio.github.io/RuHere/reference/plot_env_bins.md)
function.

## Usage

``` r
get_env_bins(
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  env_layers,
  n_bins = 5
)
```

## Arguments

- occ:

  (data.frame or data.table) a data frame containing the occurrence
  records for a **single species**. Must contain columns for species,
  longitude, and latitude.

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

## Value

A list with:

- **data**: a data frame including extracted environmental values, bin
  indices, and a unique `block_id` for each record.

- **breaks**: a named list of numeric vectors containing the break
  points for each variable (used by
  [`plot_env_bins()`](https://wevertonbio.github.io/RuHere/reference/plot_env_bins.md)).

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
# Get bins
b <- get_env_bins(occ = occ, env_layers = r, n_bins = 5)
```
