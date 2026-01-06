# Check if the records fall in the state assigned in the metadata

Check if the records fall in the state assigned in the metadata

## Usage

``` r
check_states(
  occ,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  state_column,
  distance = 5,
  try_to_fix = FALSE,
  progress_bar = FALSE,
  verbose = TRUE
)
```

## Arguments

- occ:

  (data.frame) a dataset with occurrence records, preferably with
  country information standardized using
  [`standardize_states()`](https://wevertonbio.github.io/RuHere/reference/standardize_states.md).

- long:

  (character) column name with longitude. Default is 'decimalLongitude'.

- lat:

  lat (character) column name with latitude. Default is
  'decimalLatitude'.

- state_column:

  (character) column name containing the state information.

- distance:

  (numeric) maximum distance (in kilometers) a record can fall outside
  the state assigned in the `state_column`. Default is `5`.

- try_to_fix:

  (logical) whether to check if coordinates are inverted or transposed
  (see
  [`fix_states()`](https://wevertonbio.github.io/RuHere/reference/fix_states.md)
  for details). If `TRUE`, coordinates identified as inverted or
  transposed will be corrected. Default is `FALSE`.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) whether to print messages about function progress. Default
  is `TRUE`.

## Value

The original `occ` data.frame with an additional column
(`correct_state`) indicating whether each record falls within the state
specified in the metadata (`TRUE`) or not (`FALSE`).

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere") #Import data example
# Subset occurrences for Araucaria angustifolia
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Standardize country names
occ_country <- standardize_countries(occ = occ,
                                     return_dictionary = FALSE)
#> Error in standardize_countries(occ = occ, return_dictionary = FALSE): object 'unique_states' not found
# Standardize state names
occ_state <- standardize_states(occ = occ_country,
                                country_column = "country_suggested",
                                return_dictionary = FALSE)
#> Error: object 'occ_country' not found
# Check whether records fall within assigned states
occ_state_checked <- check_states(occ = occ_state,
                                    state_column = "state_suggested")
#> Error: object 'occ_state' not found
```
