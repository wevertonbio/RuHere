# Identify and remove invalid coordinates

This function identifies and removes invalid geographic coordinates,
including non-numeric values, NA or empty values, and coordinates
outside the valid range for Earth (latitude \> 90 or \< -90, and
longitude \> 180 or \< -180).

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

  (character) column name in `occ` with the longitude.

- lat:

  (character) column name in `occ` with the latitude.

- return_invalid:

  (logical) whether to return a list containing records that passed and
  failed this test. Default is TRUE.

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
  is `TRUE`.

## Value

The input data.frame with an additional logical column indicating
whether each record passed (`TRUE`) or failed (`FALSE`) the coordinate
quality check (i.e., is not missing, non-numeric, or outside the
possible range for Earth: latitude between -90 and 90, longitude between
-180 and 180). As with all other flagging functions in RuHere, `TRUE`
indicates that the record passed this test and is eligible for
retention; `FALSE` indicates it failed and is flagged as potentially
problematic, and can be removed using
[`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md).

If `return_invalid = FALSE`, returns the occurrence dataset containing
only records flagged as `TRUE`. If `return_invalid = TRUE` (default),
returns a list with two elements:

- `valid` – the dataset with records flagged as `TRUE`.

- `invalid` – the dataset with records flagged as `FALSE`.

## Examples

``` r
# Create fake data example
occ <- data.frame("species" = "spp",
                  "decimalLongitude" = c(10, -190, 20, 50, NA),
                  "decimalLatitude" = c(20, 20, 240, 50, NA))
# Split valid and invalid coordinates
occ_valid <- remove_invalid_coordinates(occ)
```
