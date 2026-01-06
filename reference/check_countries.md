# Check if the records fall in the country assigned in the metadata

Check if the records fall in the country assigned in the metadata

## Usage

``` r
check_countries(
  occ,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  country_column,
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
  [`standardize_countries()`](https://wevertonbio.github.io/RuHere/reference/standardize_countries.md).

- long:

  (character) column name with longitude. Default is 'decimalLongitude'.

- lat:

  lat (character) column name with latitude. Default is
  'decimalLatitude'.

- country_column:

  (character) column name containing the country information.

- distance:

  (numeric) maximum distance (in kilometers) a record can fall outside
  the country assigned in the `country_column`. Default is `5`.

- try_to_fix:

  (logical) whether to check if coordinates are inverted or transposed
  (see
  [`fix_countries()`](https://wevertonbio.github.io/RuHere/reference/fix_countries.md)
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
(`correct_country`) indicating whether each record falls within the
country specified in the metadata (`TRUE`) or not (`FALSE`).

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere") #Import data example
# Standardize country names
occ_country <- standardize_countries(occ = occurrences,
                                     return_dictionary = FALSE)
#> Error in standardize_countries(occ = occurrences, return_dictionary = FALSE): object 'unique_states' not found
# Check whether records fall within assigned countries
occ_country_checked <- check_countries(occ = occ_country,
                                       country_column = "country_suggested")
#> Error: object 'occ_country' not found
```
