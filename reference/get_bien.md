# Download occurrence records from BIEN

Wrapper function to access and download occurrence records from the
Botanical Information and Ecology Network (BIEN) database. It provides a
unified interface to query BIEN data by species, genus, family, or by
geographic or political boundaries.

## Usage

``` r
get_bien(by = "species", cultivated = FALSE,
new.world = NULL, all.taxonomy = FALSE, native.status = FALSE,
natives.only = TRUE, observation.type = FALSE, political.boundaries = TRUE,
collection.info = TRUE, only.geovalid = TRUE, min.lat = NULL, max.lat = NULL,
min.long = NULL, max.long = NULL, species = NULL, genus = NULL,
country = NULL, country.code = NULL, state = NULL, county = NULL,
state.code = NULL, county.code = NULL, family = NULL, sf = NULL, dir,
filename = "bien_output", file.format = "csv", compress = FALSE,
save = FALSE, verbose = TRUE, ...)
```

## Arguments

- by:

  (character) type of query to perform (`"box"`, `"country"`,
  `"county"`, `"family"`, `"genus"`, `"records_per_species"`,
  `"species"`, `"sf"`, or `"state"`). Default is `species`.

- cultivated:

  (logical) whether to include cultivated records or exclude them.
  Default is `FALSE`.

- new.world:

  (logical) if `TRUE`, restricts records to the New World, if `FALSE`,
  to the Old World, and if `NULL`, no restriction. Default is `NULL`.

- all.taxonomy:

  (logical) if `TRUE`, returns all taxonomic levels available,
  otherwise, limits results to accepted names. Default is `FALSE`.

- native.status:

  (logical) if `TRUE`, includes information about native versus
  non-native status of occurrences. Default is `FALSE`.

- natives.only:

  (logical) if `TRUE`, restricts results to native species only. Default
  is `TRUE`.

- observation.type:

  (logical) if `TRUE`, includes information on observation types.
  Default is `FALSE`.

- political.boundaries:

  (logical) if `TRUE`, restricts the search to defined political
  boundaries. Default is `TRUE`.

- collection.info:

  (logical) if `TRUE`, includes collection-level metadata. Default is
  `TRUE`.

- only.geovalid:

  (logical) if `TRUE`, restricts output to georeferenced and spatially
  valid records. Default is `TRUE`.

- min.lat:

  (numeric) the minimum latitude (in decimal degrees) for a bounding-box
  query when `by = "box"`.

- max.lat:

  (numeric) the maximum latitude (in decimal degrees) for a bounding-box
  query when `by = "box"`.

- min.long:

  (numeric) the minimum longitude (in decimal degrees) for a
  bounding-box query when `by = "box"`.

- max.long:

  (numeric) the maximum longitude (in decimal degrees) for a
  bounding-box query when `by = "box"`. Ignored otherwise. Default is
  `NULL`.

- species:

  (character) species name(s) to query when `by = "species"` or
  `"records_per_species"`. Default is `NULL`.

- genus:

  (character) genus name(s) to query when `by = "genus"`. Default is
  `NULL`.

- country:

  (character) country name when `by = "country"`, `"state"`, or
  `"county"`. Default is `NULL`.

- country.code:

  (character) two-letter ISO country code corresponding to `country`.
  Default is `NULL`.

- state:

  (character) state or province name when `by = "state"` or `"county"`.
  Default is `NULL`.

- county:

  (character) county or equivalent subdivision name when
  `by = "county"`. Default is `NULL`.

- state.code:

  (character) state or province code corresponding to `state`. Default
  is `NULL`.

- county.code:

  (character) county or equivalent subdivision code corresponding to
  `county`. Default is `NULL`.

- family:

  (character) family name(s) to query when `by = "family"`. Default is
  `NULL`.

- sf:

  (object of class `sf`) a spatial object defining an area of interest
  when `by = "sf"`. Default is `NULL`.

- dir:

  (character) directory path where the file will be saved. Required if
  `save = TRUE`.

- filename:

  (character) name of the output file without extension. Default is
  `"bien_output"`.

- file.format:

  (character) file format for saving output (`"csv"`, `"rds"`). Default
  is `"csv"`.

- compress:

  (logical) if `TRUE` and `save = TRUE`, compresses the output file as
  .csv.zip. Default is `FALSE`.

- save:

  (logical) if `TRUE`, saves the results to a CSV file. Default is
  `FALSE`.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

- ...:

  additional arguments passed to the underlying BIEN function.

## Value

A `data.frame` containing BIEN occurrence records that match the
specified query. The structure and available columns depend on the
chosen `by` value and the corresponding BIEN function.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example: download occurrence records for a single species
res_test <- get_bien(
    by = "species",
    species = "Paubrasilia echinata",
    cultivated = TRUE,
    native.status = TRUE,
    observation.type = TRUE,
    only.geovalid = TRUE
)
} # }
```
