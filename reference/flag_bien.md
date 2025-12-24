# Identify records outside natural ranges according to BIEN

Flags (validates) occurrence records based on known distribution data
from the Botanical Information and Ecology Network (BIEN) data. This
function checks if an occurrence point for a given species falls within
its documented distribution, allowing for user-defined buffers around
the region. Records are flagged as valid (`TRUE`) if they fall inside
the documented distribution (plus optional buffer) for the species in
the BIEN dataset.

## Usage

``` r
flag_bien(
  data_dir,
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  buffer = 10,
  progress_bar = FALSE,
  verbose = TRUE
)
```

## Arguments

- data_dir:

  (character) **Required** directory path where the `BIEN` data is saved

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

- buffer:

  (numeric) buffer distance (in kilometers) to be applied around the
  region of distribution. Default is 20 km.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

## Value

A `data.frame` that is the original `occ` data frame augmented with a
new column named `bien_flag`. This column is logical (`TRUE`/`FALSE`)
indicating whether the record falls within the expected distribution
(plus buffer) based on the `BIEN` data. Records for species not found in
the `BIEN` data will have `NA` in the `bien_flag` column.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Filter occurrences for golden trumpet tree
occ <- occurrences[occurrences$species == "Handroanthus serratifolius", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'bien_here()' beforehand to download the necessary data files
dataset_dir <- system.file("extdata/datasets", package = "RuHere")

# Flag records using BIEN specialist information
occ_bien <- flag_bien(data_dir = dataset_dir, occ = occ)
#> Checking the distribution from 1 of 1 species
```
