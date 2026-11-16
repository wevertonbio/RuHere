# Standardize country names

This function standardizes country names using both names and codes
present in a specified column.

## Usage

``` r
standardize_countries(
  occ,
  country_column = "country",
  max_distance = 0.1,
  user_dictionary = NULL,
  lookup_na_country = FALSE,
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

- country_column:

  (character) the column name containing the country information.
  Default is `"country"`.

- max_distance:

  (numeric) maximum allowed distance (as a fraction) when searching for
  suggestions for misspelled country names. Can be any value between 0
  and 1. Higher values return more suggestions. See
  [`agrep()`](https://rdrr.io/r/base/agrep.html) for details. Default is
  `0.1`.

- user_dictionary:

  (data.frame) optional data.frame with two columns: `'country_name'`
  and `'country_suggested'`. If provided, this dictionary will be
  combined with the package's default country dictionary
  ([`RuHere::country_dictionary`](https://wevertonbio.github.io/RuHere/reference/country_dictionary.md)).
  Default is `NULL`.

- lookup_na_country:

  (logical) whether to extract the country from coordinates when the
  country column has missing values or ambiguous fuzzy matches. If
  `TRUE`, longitude and latitude columns must be provided. Default is
  `FALSE`.

- long:

  (character) column name with longitude. Only applicable if
  `lookup_na_country = TRUE`. Default is `"decimalLongitude"`.

- lat:

  (character) column name with latitude. Only applicable if
  `lookup_na_country = TRUE`. Default is `"decimalLatitude"`.

- return_dictionary:

  (logical) whether to return the dictionary of countries that were
  (fuzzy) matched. Default is `TRUE`.

## Value

If `return_dictionary = FALSE`, returns the original `occ` `data.frame`
augmented with a `country_suggested` column (and `country_source` if
`lookup_na_country = TRUE`). If `return_dictionary = TRUE`, returns a
list with two elements:

- occ:

  The original `occ` data.frame with the `country_suggested` column.

- report:

  A data.frame with the original country names and the suggested
  matches.

## Details

Country names are first standardized by exact matching against a list of
country names in several languages from
`rnaturalearthdata::map_units110`. Any unmatched names are then
processed using a fuzzy matching algorithm to find potential candidates
for misspelled country names. When fuzzy matching yields multiple
candidate countries for a single input string, the ambiguity is resolved
using geographic coordinates if `lookup_na_country = TRUE` (retaining
the candidate that matches the point's location), or set to `NA` when
coordinates are unavailable or `lookup_na_country = FALSE`. If unmatched
names remain and `lookup_na_country = TRUE`, the country is extracted
from coordinates using a map retrieved from
`rnaturalearthdata::map_units110`.

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
```
