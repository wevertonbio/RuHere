# Select Spatially Thinned Occurrences Using Moran's I Autocorrelation

This function evaluates multiple geographically thinned datasets
(produced using different thinning distances) and selects the one that
best balances **low spatial autocorrelation** and **number of retained
records**.

For each thinning distance provided in `d`, the function computes
Moran's I for the selected environmental variables and summarizes
autocorrelation using a chosen statistic (mean, median, minimum, or
maximum). The best thinning level is then selected according to criteria
described in *Details*.

## Usage

``` r
flag_geo_moran(
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  d,
  distance = "haversine",
  moran_summary = "mean",
  min_records = 10,
  min_imoran = 0.1,
  prioritary_column = NULL,
  decreasing = TRUE,
  env_layers,
  do_pca = FALSE,
  mask = NULL,
  pca_buffer = 1000,
  return_all = FALSE,
  verbose = TRUE
)
```

## Arguments

- occ:

  (data.frame or data.table) a data frame containing the occurrence
  records for a **single species**. Must contain columns for species,
  longitude, and latitude.

- species:

  (character) the name of the column in `occ` that contains the species
  scientific names. Default is `"species"`.

- long:

  (character) the name of the column in `occ` that contains the
  longitude values. Default is `"decimalLongitude"`.

- lat:

  (character) the name of the column in `occ` that contains the latitude
  values. Default is `"decimalLatitude"`.

- d:

  (numeric) vector of thinning distances in **kilometers** (e.g., c(5,
  10, 15, 20)).

- distance:

  (character) distance metric used to compute the weight matrix for
  Moran's I. One of `"haversine"` or `"euclidean"`. Default:
  `"haversine"`.

- moran_summary:

  (character) summary statistic used to select the best thinning
  distance. One of `"mean"`, `"median"`, `"max"`, or `"min"`. Default:
  `"mean"`.

- min_records:

  (numeric) minimum number of records required for a dataset to be
  considered. Default: `10`.

- min_imoran:

  (numeric) minimum Moran's I required to avoid selecting datasets with
  extremely low spatial autocorrelation. Default: `0.1`.

- prioritary_column:

  (character) name of a numeric columns in `occ`to define retention
  priority (e.g., quality score, year). See details.

- decreasing:

  (logical) whether to sort records in decreasing order using the
  `prioritary_column` (e.g., from most recent to oldest when the
  variable is `"year"`). Only applicable when `prioritary_column` is not
  `NULL`. Default is `TRUE`.

- env_layers:

  (SpatRaster) object containing environmental variables for computing
  Moran's I.

- do_pca:

  (logical) whether environmental variables should be summarized using
  PCA before computing Moran's I. Default: `FALSE`. See details.

- mask:

  (SpatVector or SpatExtent) optional spatial object to mask the
  `env_layers` before computing PCA. Only applicable if `do_pca = TRUE`.
  Default is NULL.

- pca_buffer:

  (numeric) buffer width (km) used when PCA is computed from the convex
  hull of records. Ignored if `mask` is provided. Default: `1000`.

- return_all:

  (logical) whether to return the full list of all thinned datasets.
  Default: `FALSE`.

- verbose:

  (logical) whether to print messages about the progress. Default is
  `TRUE`

## Value

A list with:

- **occ**: the selected thinned occurrence dataset with the column
  `thin_geo_flag`indicating whether each record is retained (`TRUE`) or
  flagged.

- **imoran**: a table summarizing Moran's I for each thinning distance

- **distance**: the thinning distance that produced the selected dataset

- **moran_summary**: the summary statistic used to select the dataset

- **all_thined**: (optional) list of thinned datasets for all distances.
  Only returned if `return_all` was set to `TRUE`

## Details

This function is inspired by the approach used in Velazco et al. (2021),
extending the procedure by allowing:

- prioritization of records based on a user-defined variable (e.g.,
  year)

- optional PCA transformation of environmental layers

- selection rules that prevent datasets with too few records or
  extremely low Moran's I from being chosen.

**Procedure overview**

1.  For each distance in `d`, generate a spatially thinned dataset using
    [`thin_geo()`](https://wevertonbio.github.io/RuHere/reference/thin_geo.md)
    function.

2.  Extract environmental values for the retained records.

3.  Compute Moran's I for each environmental variable.

4.  Summarize autocorrelation per dataset (mean, median, min, or max).

5.  Apply the selection criteria:

    - Keep only datasets with at least `min_records` records.

    - Keep only datasets with Moran's I higher than `min_imoran`.

    - Round Moran's I to two decimal places and select the dataset with
      the **25th lowest** autocorrelation.

    - If more than on dataset is selected, choose the dataset retaining
      **more records**.

    - If still tied, choose the dataset with the **smallest thinning
      distance**.

**Distance matrix for Moran's I** Moran's I requires a weight matrix
derived from pairwise distances among records. Two distance types are
available:

- `"haversine"`: geographic distance computed with
  [`fields::rdist.earth()`](https://rdrr.io/pkg/fields/man/rdist.earth.html)
  (default; recommended for longitude/latitude coordinates)

- `"euclidean"`: Euclidean distance computed with
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html)

**Environmental PCA (optional)** If `do_pca = TRUE`, the environmental
layers are summarized using PCA before Moran's I is computed.

- If `mask` is provided, PCA is computed on masked layers.

- Otherwise, a convex hull around the records is buffered by
  `pca_buffer` kilometers to define the PCA area.

- It will select the axis that together explain more than 90% of the
  variation.

## References

- Velazco, S. J. E., Svenning, J. C., Ribeiro, B. R., &
  Laureto, L. M. O. (2021). On opportunities and threats to conserve the
  phylogenetic diversity of Neotropical palms. Diversity and
  Distributions, 27(3), 512–523. https://doi.org/10.1111/ddi.13215

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Subset occurrences from Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Select thinned occurrences
occ_geo_moran <- flag_geo_moran(occ = occ, d = c(5, 10, 20, 30),
                                  env_layers = r)
#> Filtering records...
#> Calculating spatial autocorrelation using Moran Index...
# Selected distance
occ_geo_moran$distance
#> [1] "30"
# Number of flagged and unflagged records
sum(occ_geo_moran$occ$thin_geo_flag) #Retained
#> [1] 297
sum(!occ_geo_moran$occ$thin_geo_flag) #Flagged for thinning out
#> [1] 627
# Results os the spatial autocorrelation analysis
occ_geo_moran$imoran
#>                   species Distance     bio_1     bio_7    bio_12 median_moran
#> 5  Araucaria angustifolia        5 0.1678046 0.3300641 0.1719100    0.1719100
#> 10 Araucaria angustifolia       10 0.1511561 0.3003992 0.1665782    0.1665782
#> 20 Araucaria angustifolia       20 0.1318548 0.2699369 0.1473847    0.1473847
#> 30 Araucaria angustifolia       30 0.1072336 0.2590974 0.1499994    0.1499994
#>    mean_moran min_moran max_moran n_filtered all_records prop_lost
#> 5   0.2232595 0.1678046 0.3300641        836         924 0.0952381
#> 10  0.2060445 0.1511561 0.3003992        621         924 0.3279221
#> 20  0.1830588 0.1318548 0.2699369        407         924 0.5595238
#> 30  0.1721101 0.1072336 0.2590974        297         924 0.6785714
```
