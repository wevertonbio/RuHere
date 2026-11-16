# Standardize state names

This function standardizes state names using both names and codes
present in a specified column.

## Usage

``` r
standardize_states(
  occ,
  state_column = "stateProvince",
  country_column = "country_suggested",
  max_distance = 0.1,
  lookup_na_state = FALSE,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  return_dictionary = TRUE
)
```

## Arguments

- occ:

  (data.frame or data.table) a dataset with occurrence records,
  preferably standardized using
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md).

- state_column:

  (character) the column name containing the state information. Default
  is `"stateProvince"`.

- country_column:

  (character) the column name containing the country information.
  Default is `"country_suggested"`.

- max_distance:

  (numeric) maximum allowed distance (as a fraction) when searching for
  suggestions for misspelled state names. Can be any value between 0
  and 1. Higher values return more suggestions. See
  [`agrep()`](https://rdrr.io/r/base/agrep.html) for details. Default is
  `0.1`.

- lookup_na_state:

  (logical) whether to extract the state from coordinates when the state
  column has missing values or ambiguous fuzzy matches. If `TRUE`,
  longitude and latitude columns must be provided. Default is `FALSE`.

- long:

  (character) column name with longitude. Only applicable if
  `lookup_na_state = TRUE`. Default is `"decimalLongitude"`.

- lat:

  (character) column name with latitude. Only applicable if
  `lookup_na_state = TRUE`. Default is `"decimalLatitude"`.

- return_dictionary:

  (logical) whether to return the dictionary of states that were (fuzzy)
  matched. Default is `TRUE`.

## Value

If `return_dictionary = FALSE`, returns the original `occ` `data.frame`
augmented with a `state_suggested` column (and `state_source` if
`lookup_na_state = TRUE`). If `return_dictionary = TRUE`, returns a list
with two elements:

- occ:

  The original `occ` data.frame with the `state_suggested` column.

- report:

  A data.frame with the original state names and the suggested matches.

## Details

State names are first standardized by exact matching against a list of
state names in several languages from `rnaturalearthdata::states50`. Any
unmatched names are then processed using a fuzzy matching algorithm to
find potential candidates for misspelled state names. When fuzzy
matching yields multiple candidate states for a single
`(state, country)` pair, the ambiguity is resolved using geographic
coordinates if `lookup_na_state = TRUE` (retaining the candidate that
matches the point's location), or set to `NA` when coordinates are
unavailable or `lookup_na_state = FALSE`. If unmatched names remain and
`lookup_na_state = TRUE`, the state is extracted from coordinates using
a map retrieved from `rnaturalearthdata::states50`.

## Examples

``` r
# Import and standardize GBIF
data("occ_gbif", package = "RuHere")
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
# Import and standardize SpeciesLink
data("occ_splink", package = "RuHere")
splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
# Import and standardize BIEN
data("occ_bien", package = "RuHere")
bien_standardized <- format_columns(occ_bien, metadata = "bien")
# Import and standardize idigbio
data("occ_idig", package = "RuHere")
idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
# Merge all
all_occ <- bind_here(gbif_standardized, splink_standardized,
                     bien_standardized, idig_standardized)
# Standardize countries
occ_standardized <- standardize_countries(occ = all_occ)
# Standardize states
occ_standardized2 <- standardize_states(occ = occ_standardized$occ)
```
