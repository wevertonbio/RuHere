# Select Environmentally Thinned Occurrences Using Moran's I Autocorrelation

This function evaluates multiple environmentally thinned datasets
(produced using different number of blocks) and selects the one that
best balances **low spatial autocorrelation** and **number of retained
records**.

For each number of bins provided in `n_bins`, the function computes
Moran's I for the selected environmental variables and summarizes
autocorrelation using a chosen statistic (mean, median, minimum, or
maximum). The best thinning level is then selected according to criteria
described in *Details*.

## Usage

``` r
flag_env_moran(
  occ,
  species = "species",
  long = "decimalLongitude",
  lat = "decimalLatitude",
  env_layers,
  n_bins,
  distance = "haversine",
  moran_summary = "mean",
  min_records = 10,
  min_imoran = 0.1,
  prioritary_column = NULL,
  decreasing = TRUE,
  do_pca = FALSE,
  mask = NULL,
  pca_buffer = 1000,
  flag_for_NA = FALSE,
  return_all = FALSE
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

- env_layers:

  (SpatRaster) object containing environmental variables for splitting
  in `n_bins` and for computing Moran's I.

- n_bins:

  (numeric) vector of number of bins into which each environmental
  variable will be divided (e.g., c(5, 10, 15, 20)).

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

- flag_for_NA:

  (logical) whether to treat records falling in `NA` cells of
  `env_layers` as valid (`TRUE`) or invalid (`FALSE`). Default is
  `FALSE`.

- return_all:

  (logical) whether to return the full list of all thinned datasets.
  Default is `FALSE`.

## Value

A list with:

- **occ**: the selected thinned occurrence dataset with the column
  `thin_env_flag`indicating whether each record is retained (`TRUE`) or
  flagged as redundant (`FALSE`) in the environmental space .

- **imoran**: a table summarizing Moran's I for each thinning distance

- **n_bins**: the number of bins that produced the selected dataset

- **moran_summary**: the summary statistic used to select the dataset

- **all_thined**: (optional) list of thinned datasets for all bin
  numbers. Only returned if `return_all` was set to `TRUE`

## Details

This function is inspired by the approach used in Velazco et al. (2020),
extending the procedure by allowing:

- prioritization of records based on a user-defined variable (e.g.,
  year)

- optional PCA transformation of environmental layers

- selection rules that prevent datasets with too few records or
  extremely low Moran's I from being chosen.

**Procedure overview**

1.  For each bin number in `n_bins`, generate a spatially thinned
    dataset using
    [`thin_env()`](https://wevertonbio.github.io/RuHere/reference/thin_env.md)
    function.

2.  Extract environmental values for the retained records.

3.  Compute Moran's I for each environmental variable.

4.  Summarize autocorrelation per dataset (mean, median, min, or max).

5.  Apply the selection criteria:

    - Keep only datasets with at least `min_records` records.

    - Keep only datasets with Moran's I ≥ `min_imoran`.

    - Round Moran's I to two decimal places and select the dataset with
      the **25th lowest** autocorrelation.

    - If more than on dataset is selected, choose the dataset retaining
      **more records**.

    - If still tied, choose the dataset with the **largest number of
      bins**.

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
occ_env_moran <- flag_env_moran(occ = occ,
                                  n_bins = c(5, 10, 20, 30, 40, 50),
                                  env_layers = r)
#> Filtering records...
#> Calculating spatial autocorrelation using Moran Index...
# Selected number of bins
occ_env_moran$n_bins
#> [1] "50"
# Number of flagged and unflagged records
sum(occ_env_moran$occ$thin_env_flag) #Retained
#> [1] 757
sum(!occ_env_moran$occ$thin_env_flag) #Flagged for thinning out
#> [1] 687
# Results os the spatial autocorrelation analysis
occ_env_moran$imoran
#>                   species n_bins     bio_1     bio_7    bio_12 median_moran
#> 5  Araucaria angustifolia      5 0.2577166 0.3304537 0.1850860    0.2577166
#> 10 Araucaria angustifolia     10 0.2096196 0.3679785 0.1804700    0.2096196
#> 20 Araucaria angustifolia     20 0.1779664 0.3634437 0.1921568    0.1921568
#> 30 Araucaria angustifolia     30 0.1804510 0.3634221 0.1952271    0.1952271
#> 40 Araucaria angustifolia     40 0.1915694 0.3524565 0.1911581    0.1915694
#> 50 Araucaria angustifolia     50 0.1882129 0.3498225 0.1909865    0.1909865
#>    mean_moran min_moran max_moran n_filtered all_records prop_lost
#> 5   0.2577521 0.1850860 0.3304537         57        1444 0.9605263
#> 10  0.2526894 0.1804700 0.3679785        199        1444 0.8621884
#> 20  0.2445223 0.1779664 0.3634437        508        1444 0.6481994
#> 30  0.2463667 0.1804510 0.3634221        666        1444 0.5387812
#> 40  0.2450613 0.1911581 0.3524565        721        1444 0.5006925
#> 50  0.2430073 0.1882129 0.3498225        757        1444 0.4757618
```
