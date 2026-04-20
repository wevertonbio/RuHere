# Amphibian communities from the Atlantic Forest

A data set of amphibian communities from the Atlantic Forests of South
America sourced from Vancine et al. (2018).

## Usage

``` r
atlantic_amphibians
```

## Format

A `data.table` or `data.frame` with 8,254 rows and 3 columns:

- species:

  Character. Scientific name of the virtual species.

- decimalLongitude:

  Numeric. Georeferenced longitude in decimal degrees.

- decimalLatitude:

  Numeric. Georeferenced latitude in decimal degrees.

## References

Vancine et al. 2018. ATLANTIC AMPHIBIANS: a data set of amphibian
communities from the Atlantic Forests of South America. Ecology, 99(7),
1692-1692. doi:10.1002/ecy.2392

## See also

[`inventory_completeness()`](https://wevertonbio.github.io/RuHere/reference/inventory_completeness.md)

## Examples

``` r
# First rows
head(atlantic_amphibians)
#> # A tibble: 6 × 3
#>   species                 decimalLongitude decimalLatitude
#>   <chr>                              <dbl>           <dbl>
#> 1 Rhinella rubescens                 -43.5           -20.1
#> 2 Boana albopunctata                 -43.5           -20.1
#> 3 Boana faber                        -43.5           -20.1
#> 4 Bokermannohyla martinsi            -43.5           -20.1
#> 5 Dendropsophus minutus              -43.5           -20.1
#> 6 Bokermannohyla nanuzae             -43.5           -20.1
```
