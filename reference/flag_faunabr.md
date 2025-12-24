# Identify records outside natural ranges according to Fauna do Brasil

Flags (validates) occurrence records based on known distribution data
from the Catálogo Taxônomico da Fauna do Brasil (faunabr) data. This
function checks if an occurrence point for a given species falls within
its documented distribution, allowing for user-defined buffers around
Brazilian states, or the entire country. Records are flagged as valid
(`TRUE`) if they fall within the specified range for the distribution
information available in the `faunabr` data.

## Usage

``` r
flag_faunabr(
  data_dir,
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  origin = NULL,
  by_state = TRUE,
  buffer_state = 20,
  by_country = TRUE,
  buffer_country = 20,
  keep_columns = TRUE,
  spat_state = NULL,
  spat_country = NULL,
  progress_bar = FALSE,
  verbose = FALSE
)
```

## Arguments

- data_dir:

  (character) **Required** directory path where the `faunabr` data is
  saved.

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

  (character) filter the `faunabr` data by origin type before checking
  (`"native"`, `"cryptogenic"`, or `"exotic"`). Default is `NULL` (no
  filtering).

- by_state:

  (logical) if `TRUE`, flags records based on their distance to known
  Brazilian state distributions. Default is `TRUE`.

- buffer_state:

  (numeric) buffer distance (in kilometers) to be applied around the
  known state distribution boundaries. Records within this distance are
  considered valid. Default is 20 km.

- by_country:

  (logical) if `TRUE`, flags records based on their distance to country
  distributions. Default is `TRUE`.

- buffer_country:

  (numeric) buffer distance (in kilometers) to be applied around the
  country boundaries. Records within this distance are considered valid.
  Default is 20 km.

- keep_columns:

  (logical) if `TRUE`, the returned data frame contains all original
  columns from `occ`. If `FALSE`, it returns only the key columns and
  the flag. Default is `TRUE`.

- spat_state:

  (SpatVector) a SpatVector of the Brazilian states. By default, it uses
  the SpatVector provided by geobr::read_state(). It can be another
  Spatvector, but the structure must be identical to 'faunabr::states',
  with a column called "abbrev_state" identifying the states codes.

- spat_country:

  (SpatVector) a SpatVector of the world countries. By default, it uses
  the SpatVector provided by rnaturalearth::ne_countries. It can be
  another Spatvector, but the structure must be identical to
  'faunabr::world_fauna', with a column called "country_code"
  identifying the country codes.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `FALSE`.

## Value

\#' A `data.frame` that is the original `occ` data frame augmented with
a new column named `faunabr_flag`. This column is logical
(`TRUE`/`FALSE`) indicating whether the record falls within the expected
distribution (plus buffer) based on the `faunabr` data. Records for
species not found in the `faunabr` data will have `NA` in the
`faunabr_flag` column.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Get only occurrences from Azure Jay
occ <- occurrences[occurrences$species == "Cyanocorax caeruleus", ]
# Set folder where distributional datasets were saved
# Here, just a sample provided in the package
# You must run 'faunabr_here()' beforehand to download the necessary data files for your species
dataset_dir <- system.file("extdata/datasets", package = "RuHere")
# Flag records using faunabr specialist information
occ_fauna <- flag_faunabr(data_dir = dataset_dir, occ = occ)
```
