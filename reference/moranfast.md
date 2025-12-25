# Fast Moran's I Autocorrelation Index

This function computes Moran's I autocorrelation coefficient for a
numeric vector `x` using a matrix of weights. The method follows
Gittleman and Kot (1990). This function is an implementation of
[`ape::Moran.I()`](https://rdrr.io/pkg/ape/man/MoranI.html), but
rewritten in C++ to be substantially faster and more memory-efficient.

## Usage

``` r
moranfast(
  x,
  weight,
  na_rm = TRUE,
  scaled = FALSE,
  alternative = c("two.sided")
)
```

## Arguments

- x:

  (numeric) A numeric vector (e.g., environmental values extracted from
  occurrence records).

- weight:

  (matrix) A matrix of spatial weights (e.g., a distance or
  inverse-distance matrix). The number of rows must be equal to the
  length of `x`.

- na_rm:

  (logical) whether to remove missing values from `x`. Default is
  `TRUE`.

- scaled:

  (logical) whether to scale Moran's I so that it ranges between –1 and
  +1. Default is `TRUE`.

- alternative:

  (character) The alternative hypothesis tested against the null
  hypothesis of no autocorrelation. Must be one of `"two.sided"`,
  `"less"`, or `"greater"`. Default is `"two.sided"`.

## Value

A list with the following components:

- **observed** – The observed Moran's I.

- **expected** – The expected value of Moran's I under the null
  hypothesis.

- **sd** – The standard deviation of Moran's I under the null
  hypothesis.

- **p.value** – The p-value of the test based on the chosen
  `alternative`.

## References

Gittleman, J. L., & Kot, M. (1990). Adaptation: statistics and a null
model for estimating phylogenetic effects. *Systematic Zoology*, 39(3),
227–241.

## Examples

``` r
# Load example data
data("occurrences", package = "RuHere")
# Filter occurrences of Araucaria
occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
# Load example of raster variables
data("worldclim", package = "RuHere")
# Unwrap Packed raster
r <- terra::unwrap(worldclim)
# Extract values for bio_1
bio_1 <- terra::extract(r$bio_1,
                        occ[, c("decimalLongitude", "decimalLatitude")],
                        ID = FALSE, xy = TRUE)
#Remove NAs
bio_1 <- na.omit(bio_1)
# Convert values to numeric
v <- as.numeric(bio_1$bio_1)
# Compute geographic distance matrix
d <- fields::rdist.earth(x1 = as.matrix(bio_1[, c("x", "y")]), miles = FALSE)
# Inverse-distance weights
d <- 1/d
# Fill diagonal with 0
diag(d) <- 0
# Remove finite values
d[is.infinite(d)] <- 0
# Compute Moran's I
m <- moranfast(x = v, weight = d, scale = TRUE)
# Print results
m
#> $observed
#> [1] 0.2977895
#> 
#> $expected
#> [1] -0.0007843137
#> 
#> $sd
#> [1] 0.01017309
#> 
#> $p.value
#> [1] 0
#> 
```
