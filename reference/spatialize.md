# Spatialize occurrence records

Convert a data.frame (or data.table) of occurrence records into a
**SpatVector** object.

## Usage

``` r
spatialize(
  occ,
  long = "decimalLongitude",
  lat = "decimalLatitude",
  crs = "epsg:4326",
  force_numeric = TRUE
)
```

## Arguments

- occ:

  (data.frame or data.table) a data frame containing the occurrence
  records to be flagged. Must contain columns for longitude, and
  latitude.

- long:

  (character) the name of the column in `occ` that contains the
  longitude values. Default is `"decimalLongitude"`.

- lat:

  (character) the name of the column in `occ` that contains the latitude
  values. Default is `"decimalLatitude"`.

- crs:

  (character) the coordinate reference system in one of the following
  formats: PROJ-string notation, WKT/WKT2, or
  :` (see `[`?terra::crs`](https://rspatial.github.io/terra/reference/crs.html)`). Default is "epsg:4326".`

- force_numeric:

  (logical) whether to coerce the longitude and latitude columns to
  numeric if they are not already. Default is `TRUE.`

## Value

A **SpatVector** object containing the spatialized occurrence records.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Spatialize the occurrence records
pts <- spatialize(occurrences)
# Plot the resulting SpatVector
terra::plot(pts)

```
