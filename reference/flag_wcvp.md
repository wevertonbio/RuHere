# Identify records outside natural ranges according to the World Checklist of Vascular Plants

Flags (validates) occurrence records based on known distribution data
from the World Checklist of Vascular Plants (WCVP) data. This function
checks if an occurrence point for a given species falls within its
documented distribution, allowing for user-defined buffers around the
region. Records are flagged as valid (`TRUE`) if they fall inside the
documented distribution (plus optional buffer) for the species in the
WCVP dataset.

## Usage

``` r
flag_wcvp(
  data_dir,
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  origin = "native",
  buffer = 20,
  progress_bar = FALSE,
  verbose = FALSE
)
```

## Arguments

- data_dir:

  (character) **Required** directory path where the `WCVP` data is saved

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

- origin:

  (character) vector specifying which origin categories should be
  considered as part of the species' range. Options are: `"native"`,
  `"introduced"`, `"extinct"`, `"location_doubtful"`, and `"all"`. For
  example, if `origin = "introduced"`, only regions where the species is
  considered introduced will be used to validate records. Default is
  `"native"`.

- buffer:

  (numeric) buffer distance (in kilometers) to be applied around the
  region of distribution. Default is 20 km.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) if `FALSE`, prints messages about the progress and the
  number of species being checked. Default is `FALSE`.

## Value

A `data.frame` that is the original `occ` data frame augmented with a
new column named `wcvp_flag`. This column is logical (`TRUE`/`FALSE`)
indicating whether the record falls within the expected distribution
(plus buffer) based on the `WCVP` data. Records for species not found in
the `WCVP` data will have `NA` in the `wcvp_flag` column.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Filter occurrences for Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'wcvp_here()' beforehand to download the necessary data files
dataset_dir <- system.file("extdata/datasets", package = "RuHere")

# Flag records using WCVP specialist information
occ_wcvp <- flag_wcvp(data_dir = dataset_dir, occ = occ)
```
