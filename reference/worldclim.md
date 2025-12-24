# Bioclimatic Variables from WorldClim (bio_1, bio_7, bio_12)

A `PackedSpatRaster` containing three bioclimatic variables from the
WorldClim, cropped to a region of interest South America.

## Usage

``` r
worldclim
```

## Format

A `SpatRaster` with 3 layers and the following characteristics:

- Dimensions:

  151 rows × 183 columns

- Resolution:

  0.08333333° × 0.08333333°

- Extent:

  xmin = -57.08333, xmax = -41.83333, ymin = -32.08333, ymax = -19.5

- CRS:

  WGS84 (EPSG:4326)

- Layers:

  bio_1

  :   Mean Annual Temperature (°C × 10)

  bio_7

  :   Temperature Annual Range (°C × 10)

  bio_12

  :   Annual Precipitation (mm)

## Source

<https://www.worldclim.org/>

## Details

This raster corresponds to three standard bioclimatic variables from the
**WorldClim 2.1** dataset.

## Examples

``` r
data(worldclim)
bioclim <- terra::unwrap(worldclim)
terra::plot(bioclim)
```
