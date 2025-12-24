# Identify records outside natural ranges according to Flora e Funga do Brasil

Flags (validates) occurrence records based on known distribution data
from the Flora e Funga do Brasil (florabr) data. This function checks if
an occurrence point for a given species falls within its documented
distribution, allowing for user-defined buffers around Brazilian states,
biomes, or the entire country. Records are flagged as valid (`TRUE`) if
they fall within the specified range for the distribution information
available in the `florabr` data.

## Usage

``` r
flag_florabr(
  data_dir,
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  origin = NULL,
  by_state = TRUE,
  buffer_state = 20,
  by_biome = TRUE,
  buffer_biome = 20,
  by_endemism = TRUE,
  buffer_brazil = 20,
  state_vect = NULL,
  state_column = NULL,
  biome_vect = NULL,
  biome_column = NULL,
  br_vect = NULL,
  keep_columns = TRUE,
  progress_bar = FALSE,
  verbose = FALSE
)
```

## Arguments

- data_dir:

  (character) directory path where the `florabr` data is saved
  **Required.**

- occ:

  (data.frame) a data frame containing the occurrence records to be
  flagged. Must contain columns for species, longitude, and latitude.

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

  (character or NULL) filter the `florabr` data by origin type before
  checking (`"native"`, `"cultivated"`, `"naturalized"`, `"unknown"`, or
  `"not_found_in_brazil"`). Default is `NULL` (no filtering).

- by_state:

  (logical) if `TRUE`, flags records based on their distance to known
  Brazilian state distributions. Default is `TRUE`.

- buffer_state:

  (numeric) buffer distance (in kilometers) to be applied around the
  known state distribution boundaries. Records within this distance are
  considered valid. Default is 20 km.

- by_biome:

  (logical) if `TRUE`, flags records based on their distance to known
  Brazilian biome distributions. Default is `TRUE`.

- buffer_biome:

  (numeric) buffer distance (in kilometers) to be applied around the
  known biome distribution boundaries. Records within this distance are
  considered valid. Default is 20 km.

- by_endemism:

  (logical) if `TRUE`, includes a check against the entire Brazilian
  boundary. Default is `TRUE`.

- buffer_brazil:

  (numeric) buffer distance (in kilometers) to be applied around the
  entire Brazilian boundary. Default is 20 km.

- state_vect:

  (SpatVector) qn optional custom simple features (`sf`) vector
  representing Brazilian states/regions. If `NULL`, uses the default
  data loaded by `florabr`. Default is `NULL`.

- state_column:

  (character) the name of the column in `state_vect` (or the default
  state vector) used to match distribution information. Default is
  `NULL`.

- biome_vect:

  (SpatVector) an optional custom simple features (`sf`) vector
  representing Brazilian biomes. If `NULL`, uses the default data loaded
  by `florabr`. Default is `NULL`.

- biome_column:

  (character) the name of the column in `biome_vect` (or the default
  biome vector) used to match distribution information. Default is
  `NULL`.

- br_vect:

  (SpatVector) an optional custom simple features (`sf`) vector
  representing the entire Brazilian boundary. If `NULL`, uses the
  default data loaded by `florabr`. Default is `NULL`.

- keep_columns:

  (logical) if `TRUE`, the returned data frame contains all original
  columns from `occ`. If `FALSE`, it returns only the key columns and
  the flag. Default is `TRUE`.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `FALSE`.

## Value

A `data.frame` that is the original `occ` data frame augmented with a
new column named `florabr_flag`. This column is logical (`TRUE`/`FALSE`)
indicating whether the record falls within the expected distribution
(plus buffer) based on the `florabr` data. Records for species not found
in the `florabr` data will have `NA` in the `florabr_flag` column.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'florabr_here()' beforehand to download the necessary data files for your species
dataset_dir <- system.file("extdata/datasets", package = "RuHere")

# Flag records using specialist information from Flora do Brasil
occ_flora <- flag_florabr(data_dir = dataset_dir, occ = occ)
```
