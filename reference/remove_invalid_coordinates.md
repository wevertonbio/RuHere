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

  (logical) whether to return a list containing the valid and invalid
  coordinates. Default is TRUE.

- verbose:

  (logical) whether to print messages about function progress. Default
  is `TRUE`.

## Value

If `return_invalid = FALSE`, returns the occurrence dataset containing
only valid coordinates. If `return_invalid = TRUE` (default), returns a
list with two elements:

- `valid` – the dataset with valid coordinates.

- `invalid` – the dataset with invalid coordinates removed.

## Examples

``` r
# Create fake data example
occ <- data.frame("species" = "spp",
                  "decimalLongitude" = c(10, -190, 20, 50, NA),
                  "decimalLatitude" = c(20, 20, 240, 50, NA))
# Split valid and invalid coordinates
occ_valid <- remove_invalid_coordinates(occ)
```
