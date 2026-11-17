# Identify and remove invalid coordinates

This function identifies and removes invalid geographic coordinates,
including non-numeric values, `NA`, `NaN`, `Inf`, or empty values, and
coordinates outside the valid range for Earth (latitude \> 90 or \< -90,
and longitude \> 180 or \< -180).

## Usage

``` r
remove_invalid_coordinates(
  occ,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  return_invalid = TRUE,
  save_invalid = FALSE,
  output_dir = NULL,
  overwrite = FALSE,
  output_format = ".gz",
  verbose = FALSE
)
```

## Arguments

- occ:

  (data.frame or data.table) a dataset with occurrence records.

- long:

  (character) column name in `occ` with the longitude. Default is
  `"decimalLongitude"`.

- lat:

  (character) column name in `occ` with the latitude. Default is
  `"decimalLatitude"`.

- return_invalid:

  (logical) whether to return a list containing records that passed and
  failed this test. Default is `TRUE`.

- save_invalid:

  (logical) whether to save the records that failed this test (i.e.,
  flagged as `FALSE`). If `TRUE`, an `output_dir` must be provided.
  Default is `FALSE`.

- output_dir:

  (character) path to an existing directory where records flagged as
  `FALSE` will be saved. Only used when `save_invalid = TRUE`.

- overwrite:

  (logical) whether to overwrite existing files in `output_dir`. Only
  used when `save_invalid = TRUE`. Default is `FALSE`.

- output_format:

  (character) output format for saving flagged records. Options are
  `".csv"` or `".gz"`. Only used when `save_invalid = TRUE`. Default is
  `".gz"`.

- verbose:

  (logical) whether to print messages about function progress. Default
  is `FALSE`.

## Value

If `return_invalid = FALSE`, returns an object of the same class as
`occ` (`data.frame` or `data.table`) containing only records with valid
coordinates. If `return_invalid = TRUE` (default), returns a list with
two elements:

- `valid` – the dataset with valid coordinates.

- `invalid` – the dataset with invalid coordinates (augmented with
  `invalid_coordinates = FALSE`).

## Examples

``` r
# Create fake data example
occ <- data.frame(
  species = "Araucaria angustifolia",
  decimalLongitude = c(-49.2, -190, -50.1, -48.5, NA),
  decimalLatitude  = c(-25.4, -25.0, 240, -26.1, NA)
)

# Split valid and invalid coordinates
res <- remove_invalid_coordinates(occ)
res$valid
#>                  species decimalLongitude decimalLatitude
#> 1 Araucaria angustifolia            -49.2           -25.4
#> 4 Araucaria angustifolia            -48.5           -26.1
res$invalid
#>                  species decimalLongitude decimalLatitude invalid_coordinates
#> 2 Araucaria angustifolia           -190.0             -25               FALSE
#> 3 Araucaria angustifolia            -50.1             240               FALSE
#> 5 Araucaria angustifolia               NA              NA               FALSE

# Return only valid records
occ_valid <- remove_invalid_coordinates(occ, return_invalid = FALSE)
```
