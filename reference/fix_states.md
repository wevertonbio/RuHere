# Identify and correct coordinates based on state information

This function identifies and correct inverted and transposed coordinates
based on state information.

## Usage

``` r
fix_states(
  occ,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  state_column,
  correct_state = "correct_state",
  distance = 5,
  progress_bar = FALSE,
  verbose = TRUE
)
```

## Arguments

- occ:

  (data.frame) a dataset with occurrence records, preferably with state
  information checked using `state_countries()`.

- long:

  (character) column name with longitude. Default is 'decimalLongitude'.

- lat:

  lat (character) column name with latitude. Default is
  'decimalLatitude'.

- state_column:

  (character) name of the column containing the state information.

- correct_state:

  (character) name of the column with logical value indicating whether
  each record falls within the state specified in the metadata. Default
  is 'correct_state'. See details.

- distance:

  (numeric) maximum distance (in kilometers) a record can fall outside
  the state assigned in the `state_column`. Default is `5`.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) whether to print messages about function progress. Default
  is TRUE.

## Value

The original `occ` data.frame with the coordinates in the `long` and
`lat` columns corrected, and an additional column (`state_issues`)
indicating whether the coordinates are:

- **correct**: the record falls within the assigned state;

- **inverted**: longitude and/or latitude have reversed signs;

- **swapped**: longitude and latitude are transposed (i.e., each appears
  in the other's column). **incorrect**: the record falls outside the
  assigned state and could not be corrected.

## Details

The function checks and corrects coordinate errors in occurrence records
by testing whether each point falls within the expected state polygon
(from `RuHere`’s internal world map).

The input occurrence data must contain a column (specified in the
`correct_state` argument) with logical values indicating which records
to check and fix — only those marked as FALSE will be processed. This
column can be obtained by running the
[`check_states()`](https://wevertonbio.github.io/RuHere/reference/check_states.md)
function.

It runs a series of seven tests to detect common issues such as
**inverted** signs or **swapped** latitude/longitude values. Inverted
coordinates have their signs flipped (e.g., -45 instead of 45), placing
the point in the opposite hemisphere, while swapped coordinates have
latitude and longitude values exchanged (e.g., -47, -15 instead of -15,
-47).

For each test, state borders are buffered by `distance` km to account
for minor positional errors.

The type of issue (or `"correct"`) is recorded in a new column,
`state_issues`. Records that match their assigned state after any
correction are updated accordingly, while remaining mismatches are
labeled `"incorrect"`.

This function can be used internally by
[`check_states()`](https://wevertonbio.github.io/RuHere/reference/check_states.md)
to automatically identify and fix common coordinate errors.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere") # Import example data
# Subset records of Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Standardize country names
occ_country <- standardize_countries(occ = occ,
                                     return_dictionary = FALSE)

# Standardize state names
occ_state <- standardize_states(occ = occ_country,
                                country_column = "country_suggested",
                                return_dictionary = FALSE)

# Check whether records fall within the assigned states
occ_states_checked <- check_states(occ = occ_state,
                                   state_column = "state_suggested")
#> Warning: The following states listed in the 'state_suggested' column were absent in the world map used for validation: NA, mexico
#> Testing states...
#> 9 records fall in wrong states

# Fix records with incorrect or misassigned states
occ_states_fixed <- fix_states(occ = occ_states_checked,
                               state_column = "state_suggested")
#> Task 1 of 7: testing if longitude is inverted
#> 0 coordinates with longitude inverted
#> Task 2 of 7: testing if latitude is inverted
#> 0 coordinates with latitude inverted
#> Task 3 of 7: testing if longitude and latitude are inverted
#> 1 coordinates with longitude and latitude inverted
#> Task 4 of 7: testing if longitude and latitude are swapped
#> 0 coordinates with longitude and latitude swapped
#> Task 5 of 7: testing if longitude and latitude are swapped -
#>             with latitude inverted
#> 0 coordinates with longitude and latitude swapped and latitude inverted
#> Task 6 of 7: testing if longitude and latitude are swapped - with latitude inverted
#> 0 coordinates with longitude and latitude swapped and longitude inverted
#> Task 7 of 7: testing if longitude and latitude are swapped - with longitude latitude inverted
#> 0 coordinates with longitude and latitude swapped and inverted
```
