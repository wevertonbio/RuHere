# 5. Reducing sampling bias

## Introduction

For most studies relying on primary biodiversity data, sampling bias can
be a problem because it creates clusters of points that reflect
accessible areas and the preferences of the researchers who collected
the data, rather than the ecological preferences of the species.

Two common approaches to mitigate sampling bias involve thinning
occurrence records (i.e., removing records that are close to each
other). Thinning can be performed in either geographic or environmental
space. The `RuHere` package allows the use of both approaches and also
enables combining the results of both methods.

As an example, let’s use the records of *Araucaria angustifolia*
available in the package, after removing records flagged as potentially
problematic:

``` r

# Load packages
library(RuHere)
library(terra)
#> terra 1.9.50
library(mapview)

# Import occurrence data
data("occ_flagged", package = "RuHere")
# Remove flagged records
occ <- remove_flagged(occ = occ_flagged)
# Plot records
ggmap_here(occ = occ)
```

![](sampling_bias_files/figure-html/import%20data-1.png)  

## Heatmap for occurrence data

Before starting the thinning process, let’s create a heatmap based on a
kernel density estimation of the records:

``` r

# Generate heatmap
heatmap <- spatial_kde(occ = occ, resolution = 0.2, buffer_extent = 50,
                       radius = 2, zero_as_NA = TRUE)
```

  

We can use the function
[`ggmap_here()`](https://wevertonbio.github.io/RuHere/reference/ggmap_here.md)
to plot the occurrences and the heatmap (or use
[`map_here()`](https://wevertonbio.github.io/RuHere/reference/map_here.md)
if you prefer an interactive version).

``` r

ggmap_here(occ = occ, size_points = 0.5, heatmap = heatmap)
```

![](sampling_bias_files/figure-html/unnamed-chunk-3-1.png)  

We can observe a notable hotspot with a cluster of records around
Curitiba city (capital of Paraná, southern Brazil), which indicates
strong sampling bias. Let’s evaluate the effect of thinning these
records in geographic space.

## Thinning in geographic space

The
[`thin_geo()`](https://wevertonbio.github.io/RuHere/reference/thin_geo.md)
function flags occurrence records for thinning by keeping only one
record per species within a radius of `d` kilometers. This function is
similar to
[`thin()`](https://rspatial.github.io/terra/reference/thin.html) from
the [spThin
package](https://nsojournals.onlinelibrary.wiley.com/doi/10.1111/ecog.01132),
but with an important difference: it allows specifying a priority order
for retaining records.

When a thinning distance is provided (e.g., 10 km), the function
identifies clusters of records within that distance. Within each
cluster, it retains the record with the highest priority according to
the column defined in prioritary_column (for example, using the most
recent record when `prioritary_column = "year"`), and flags all
remaining nearby records for removal. If `prioritary_column` is `NULL`,
the priority follows the original row order of the input occ data frame.

Let’s thin the records using a 10 km radius and keep the most recent
record as the priority:

``` r

# Thin records using a 10 km distance threshold
occ_thin <- thin_geo(occ = occ, d = 10, prioritary_column = "year")
sum(!occ_thin$thin_geo_flag)  # Number of records flagged for removal
#> [1] 4
```

  

The function flagged 1,860 records for removal. Let’s visualize the
flagged records using
[`map_here()`](https://wevertonbio.github.io/RuHere/reference/map_here.md)
and create a heatmap with the remaining records.

``` r

# Remove flagged records
occ_thin_geo <- remove_flagged(occ = occ_thin)
# Create heatmap
heatmap_thin_geo <- spatial_kde(occ = occ_thin_geo, resolution = 0.2, 
                                buffer_extent = 50, radius = 2, 
                                zero_as_NA = TRUE)
# Plot
ggmap_here(occ_thin, size_points = 0.5, heatmap = heatmap_thin_geo)
```

![](sampling_bias_files/figure-html/unnamed-chunk-5-1.png)  

The thinned dataset (in green) produces a more spatially uniform heatmap
and reduces the strong sampling bias around Curitiba. We explore
different distance thresholds in the next section.

  

## Selecting the best distance to thin records

A key question is determining the optimal thinning distance, since we
rarely have sufficient biological justification for choosing a specific
value. To address this issue, we adapted the approach of [Velazco et
al. (2020)](https://onlinelibrary.wiley.com/doi/10.1111/ddi.13215),
which computes spatial autocorrelation (Moran’s I) for datasets
generated using different thinning distances and selects the distance
that yields the lowest average spatial autocorrelation.

We extended this procedure by adjusting the selection rules to avoid
choosing datasets with too few records or unrealistically low Moran’s I
values. See
[`help(flag_geo_moran)`](https://wevertonbio.github.io/RuHere/reference/flag_geo_moran.md)
for full details of the selection procedure.

As an example, let’s test the effect of thinning using distances of 1,
3, 5, 7, 10, 15, 20, and 30 km. For computing spatial correlation, we
need a raster containing the environmental variables. Here, we again
specify a priority for retaining records (the most recent ones).

``` r

# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Select thinned occurrences
occ_geo_moran <- flag_geo_moran(occ = occ, 
                                d = c(1, 3, 5, 7, 10, 15, 20, 30), 
                                prioritary_column = "year",
                                env_layers = r)
#> Filtering records...
#> Calculating spatial autocorrelation using Moran Index...
```

  

The results for each tested distance are returned in the `imoran`
data.frame. It includes Moran’s I for each variable and summary
statistics across variables (mean, median, minimum, and maximum), along
with the number of retained records (`n_filtered`) and the proportion of
records flagged (`prop_lost`).

``` r

occ_geo_moran$imoran
#>                   species Distance       bio_1     bio_7    bio_12 median_moran
#> 1  Araucaria angustifolia        1  0.02364183 0.2946608 0.1627377    0.1627377
#> 3  Araucaria angustifolia        3  0.02364183 0.2946608 0.1627377    0.1627377
#> 5  Araucaria angustifolia        5  0.02364183 0.2946608 0.1627377    0.1627377
#> 7  Araucaria angustifolia        7  0.02364183 0.2946608 0.1627377    0.1627377
#> 10 Araucaria angustifolia       10  0.02364183 0.2946608 0.1627377    0.1627377
#> 15 Araucaria angustifolia       15 -0.05854502 0.1461562 0.1676072    0.1461562
#> 20 Araucaria angustifolia       20 -0.05854502 0.1461562 0.1676072    0.1461562
#> 30 Araucaria angustifolia       30 -0.08638203 0.1567395 0.1822410    0.1567395
#>    mean_moran   min_moran max_moran n_filtered all_records prop_lost
#> 1  0.16034678  0.02364183 0.2946608         28          33 0.1515152
#> 3  0.16034678  0.02364183 0.2946608         28          33 0.1515152
#> 5  0.16034678  0.02364183 0.2946608         28          33 0.1515152
#> 7  0.16034678  0.02364183 0.2946608         28          33 0.1515152
#> 10 0.16034678  0.02364183 0.2946608         28          33 0.1515152
#> 15 0.08507278 -0.05854502 0.1676072         27          33 0.1818182
#> 20 0.08507278 -0.05854502 0.1676072         27          33 0.1818182
#> 30 0.08419952 -0.08638203 0.1822410         25          33 0.2424242
```

  

The “best” distance that reduces the spatial autocorrelation without
discarding too many records was 15km. Using this threshold, 2,040
records were flagged.

``` r

# Best distance selected
occ_geo_moran$distance
#> [1] "1"
# Number of flagged records using this distance to thin
sum(!occ_geo_moran$occ$thin_geo_flag)
#> [1] 5
```

  

Visual inspection shows an even more uniform heatmap:

``` r

# Remove flagged records
occ_thin_geo_moran <- remove_flagged(occ = occ_geo_moran$occ)
# Create heatmap
heatmap_thin_geo_moran <- spatial_kde(occ = occ_thin_geo_moran, 
                                      resolution = 0.2, 
                                      buffer_extent = 50, radius = 2,
                                      zero_as_NA = TRUE)
ggmap_here(occ = occ_geo_moran$occ, size_points = 0.5, 
           heatmap = heatmap_thin_geo_moran)
```

![](sampling_bias_files/figure-html/unnamed-chunk-9-1.png)  

A potential issue when filtering records in geographic space is that two
nearby records may actually occur in distinct environmental conditions,
especially in highly heterogeneous regions. This can lead to the loss of
unique information about the species’ niche and environmental
tolerances. To address this, we can instead apply thinning in
environmental space, as explored in the next sections.  

## Thinning in environmental space

Thinning in environmental space removes records with similar
environmental conditions, representing redundant ecological information.
This is achieved by building a multidimensional grid in environmental
space, dividing each variable into `n_bins` equally sized intervals.
Each record is assigned to a unique environmental block (a combination
of bins), and records within the same block (i.e., environmentally
similar) are flagged for removal.

To illustrate how the environmental grid works, let’s use
[`get_env_bins()`](https://wevertonbio.github.io/RuHere/reference/get_env_bins.md)
with 10 bins:

``` r

# Get bins
b <- get_env_bins(occ = occ, env_layers = r, n_bins = 10)
head(b$data)
#>      bio_1  bio_7 bio_12 bio_1_bin bio_7_bin bio_12_bin block_id
#> 1       NA     NA     NA        NA        NA         NA     <NA>
#> 2       NA     NA     NA        NA        NA         NA     <NA>
#> 3       NA     NA     NA        NA        NA         NA     <NA>
#> 4       NA     NA     NA        NA        NA         NA     <NA>
#> 5 18.59283 22.753   1812         5        10          8   5_10_8
#> 6 21.34883 21.640   1669        10         9          6   10_9_6
```

  

The function returns the environmental block IDs for each record. We can
visualize the grid for any two variables:

``` r

# Plot
plot_env_bins(b, x_var = "bio_1", y_var = "bio_12",
              xlab = "Temperature", ylab = "Precipitation")
```

![](sampling_bias_files/figure-html/unnamed-chunk-11-1.png)  

We can see that several records fall into the same block, meaning they
are environmentally similar and therefore redundant. Let’s flag these
redundant records using
[`thin_env()`](https://wevertonbio.github.io/RuHere/reference/thin_env.md):

``` r

# Flag records that are close to each other in the enviromnetal space
occ_thin_env <- thin_env(occ = occ, env_layers = r, n_bins = 10, 
                         prioritary_column = "year")
# Number of flagged (redundant) records
sum(!occ_thin_env$thin_env_flag) #Number of flagged records
#> [1] 13
```

  

The function flagged 2,227 records. Let’s visualize these and create a
heatmap of the remaining data.

``` r

# Remove flagged records
occ_thinned_env <- remove_flagged(occ = occ_thin_env)
# Create heatmap
heatmap_thin_env <- spatial_kde(occ = occ_thinned_env, resolution = 0.2, 
                                buffer_extent = 50, radius = 2, 
                                zero_as_NA = TRUE)
ggmap_here(occ_thin_env, size_points = 0.5, heatmap = heatmap_thin_env)
```

![](sampling_bias_files/figure-html/unnamed-chunk-13-1.png)  

Thinning in environmental space produces a spatial pattern that differs
from the dataset filtered exclusively in geographic space.

Similar to the thinning process in geographic space, we can test
different numbers of bins (see next section).

## Selecting the best number of environmental bins

In geographic thinning, the key parameter is distance. In environmental
thinning, it is the number of bins. More bins result in finer partitions
of environmental space, reducing the chances of records falling into the
same block. As with geographic thinning, we can test multiple bin values
and select the one that reduces spatial autocorrelation without
discarding many records.

Here, we test 5, 10, 20, 30, 40, 50, 60, 70, and 80 bins:

``` r

# Select thinned occurrences
occ_env_moran <- flag_env_moran(occ = occ, 
                                n_bins = c(5, 10, 20, 30, 40, 50, 60, 70, 80), 
                                prioritary_column = "year",
                                env_layers = r)
#> Filtering records...
#> Calculating spatial autocorrelation using Moran Index...
```

  

Again, results are returned in an imoran data frame. It includes Moran’s
I for each variable and summary statistics across variables (mean,
median, minimum, and maximum), along with the number of retained records
(`n_filtered`) and the proportion of records flagged (`prop_lost`).

``` r

occ_env_moran$imoran
#>                   species n_bins      bio_1     bio_7    bio_12 median_moran
#> 5  Araucaria angustifolia      5 0.01909402 0.2841587 0.1245460    0.1245460
#> 10 Araucaria angustifolia     10 0.00526043 0.2838191 0.1159936    0.1159936
#> 20 Araucaria angustifolia     20 0.02364183 0.2946608 0.1627377    0.1627377
#> 30 Araucaria angustifolia     30 0.02364183 0.2946608 0.1627377    0.1627377
#> 40 Araucaria angustifolia     40 0.02364183 0.2946608 0.1627377    0.1627377
#> 50 Araucaria angustifolia     50 0.02364183 0.2946608 0.1627377    0.1627377
#> 60 Araucaria angustifolia     60 0.02364183 0.2946608 0.1627377    0.1627377
#> 70 Araucaria angustifolia     70 0.02364183 0.2946608 0.1627377    0.1627377
#> 80 Araucaria angustifolia     80 0.02364183 0.2946608 0.1627377    0.1627377
#>    mean_moran  min_moran max_moran n_filtered all_records prop_lost
#> 5   0.1425996 0.01909402 0.2841587         19          33 0.4242424
#> 10  0.1350244 0.00526043 0.2838191         20          33 0.3939394
#> 20  0.1603468 0.02364183 0.2946608         22          33 0.3333333
#> 30  0.1603468 0.02364183 0.2946608         22          33 0.3333333
#> 40  0.1603468 0.02364183 0.2946608         22          33 0.3333333
#> 50  0.1603468 0.02364183 0.2946608         22          33 0.3333333
#> 60  0.1603468 0.02364183 0.2946608         22          33 0.3333333
#> 70  0.1603468 0.02364183 0.2946608         22          33 0.3333333
#> 80  0.1603468 0.02364183 0.2946608         22          33 0.3333333
```

  

The “best” number of bins that reduces the spatial autocorrelation
without discarding too many records was 70. With this threshold, 1,659
records were flagged.

``` r

# Best distance selected
occ_env_moran$n_bins
#> [1] "80"
# Number of flagged records using this distance to thin
sum(!occ_env_moran$occ$thin_env_flag)
#> [1] 11
```

  

Let’s check the distribution of these records and the heatmap generated
with the unflagged records:

``` r

# Remove flagged records
occ_thin_env_moran <- remove_flagged(occ = occ_env_moran$occ)
# Create heatmap
heatmap_thin_env_moran <- spatial_kde(occ = occ_thin_env_moran, 
                                      resolution = 0.2, 
                                      buffer_extent = 50, radius = 2,
                                      zero_as_NA = TRUE)
ggmap_here(occ = occ_env_moran$occ, size_points = 0.5, 
           heatmap = heatmap_thin_env_moran)
```

![](sampling_bias_files/figure-html/unnamed-chunk-17-1.png)  

## Consensus between environmental and geographic thinning

Thinning in environmental space can suffer from the opposite issue of
geographic thinning: environmentally similar records may be
geographically far apart, potentially removing important information
about the species’ geographic range.

To address this, the
[`flag_consensus()`](https://wevertonbio.github.io/RuHere/reference/flag_consensus.md)
function can be used to flag records only when they are redundant in
both geographic and environmental space.

``` r

# Flag occurrences by thinning in geographic space
occ_geo_moran <- flag_geo_moran(occ = occ, 
                                d = c(1, 3, 5, 7, 10, 15, 20, 30), 
                                prioritary_column = "year",
                                env_layers = r)
#> Filtering records...
#> Calculating spatial autocorrelation using Moran Index...

# Flag occurrences by thinning in environmental space
occ_env_moran <- flag_env_moran(occ = occ_geo_moran$occ, 
                                n_bins = c(5, 10, 20, 30, 40, 50, 60, 70, 80), 
                                prioritary_column = "year",
                                env_layers = r)
#> Filtering records...
#> Calculating spatial autocorrelation using Moran Index...
# Get consensus
occ_consensus <- flag_consensus(occ = occ_env_moran$occ,
                                flags = c("thin_geo", "thin_env"),
                                consensus_rule = "any_true",
                                flag_name = "thin_geo_env_flag")
# Remove flagged
occ_consensus_filtered <- remove_flagged(occ = occ_consensus, flags = NULL,
                                         additional_flags = c("thin_geo_env" = "thin_geo_env_flag"))

# Create heatmap
heatmap_consensus_filtered <- spatial_kde(occ = occ_consensus_filtered,
                                          resolution = 0.2,
                                          buffer_extent = 50,
                                          radius = 2, zero_as_NA = TRUE)
```

  

Let’s visualize which records were flagged by geographic thinning,
environmental thinning, or both:

``` r

ggmap_here(occ = occ_consensus, 
          flags = c("thin_geo", "thin_env"), 
          additional_flags = "thin_geo_env_flag", 
          names_additional_flags = "Thinned geo-env", 
          col_additional_flags = "blue",
          size_points = 0.5,
          heatmap = heatmap_consensus_filtered)
```

![](sampling_bias_files/figure-html/unnamed-chunk-19-1.png)  

Red points indicate records thinned in geographic space; yellow points
indicate thinning in environmental space; and blue points indicate
records thinned in both.

Note that we retained the records flagged only in the geographic
thinning, those flagged only in the environmental thinning, and those
not flagged by either method.
