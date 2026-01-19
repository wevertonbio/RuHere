# Species Richness and Occurrence Summary Mapping

This function generates spatial grids (rasters) of species richness,
record density, or summarized biological traits from occurrence data. It
supports custom resolutions, masking, and automatic coordinate
reprojection to match reference rasters.

## Usage

``` r
richness_here(
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  raster_base = NULL,
  res = NULL,
  crs = "epsg:4326",
  mask = NULL,
  summary = "records",
  field = NULL,
  fun = mean,
  verbose = TRUE
)
```

## Arguments

- occ:

  (data.frame) a dataset containing occurrence records. Must include
  columns for species names and geographic coordinates.

- species:

  (character) the name of the column in `occ` that contains the species
  scientific names. Default is `"species"`.

- long:

  (character) the name of the column in `occ` that contains the
  longitude values. Default is `"decimalLongitude"`.

- lat:

  (character) the name of the column in `occ` that contains the latitude
  values. Default is `"decimalLatitude"`.

- raster_base:

  (SpatRaster) an optional reference raster. If provided, the output
  will match its resolution, extent, and CRS. Default is `NULL`.

- res:

  (numeric) the desired resolution (in decimal degrees if WGS84) for the
  output grid. Only used if `raster_base` is `NULL`.

- crs:

  (character) the coordinate reference system of the raster. (see
  ?terra::crs). Default is "epsg:4326". Only applicable if `raster_base`
  is not provided.

- mask:

  (SpatRaster or SpatVector) an optional layer to mask the final output.
  Default is `NULL`.

- summary:

  (character) the type of summary to calculate. Either `"records"`
  (number of occurrences per cell) or `"species"` (number of unique
  species per cell). Default is `"records"`.

- field:

  (character or named vector) columns in `occ` to summarize (e.g.,
  traits). If a named vector is provided, names must match species in
  `occ`. Only used when `summary = "species"`. Default is `NULL`.

- fun:

  (function) the function to aggregate `field` values (e.g., `mean`,
  `max`, `sum`). Default is `mean`.

- verbose:

  (logical) whether to print messages about the progress. Default is
  `TRUE`

## Value

A `SpatRaster` object representing the calculated richness, density, or
trait summary.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
occ <- occurrences

# Record density map
r_records <- richness_here(occ, res = 0.5, summary = "records")
terra::plot(r_records)


# Species richness map masked by Brazil's border
world <- terra::unwrap(RuHere::world) # Import world map
brazil <- world[world$name == "brazil",] # Subset Brazil
r_richness <- richness_here(occ, res = 1, mask = brazil, summary = "species")
terra::plot(r_richness)


# Average trait value per cell
sim_mass <- c(runif(length(unique(occ$species)), 10, 20))
names(sim_mass) <- unique(occ$species)

r_trait <- richness_here(occ, res = 0.5, summary = "species", mask = brazil,
                         field = sim_mass, fun = mean)
terra::plot(r_trait)

```
