# Flagged occurrence records of *Araucaria angustifolia*

A dataset containing the occurrence records of *Araucaria angustifolia*
after applying several of the package’s flagging and data-quality
assessment functions.

## Usage

``` r
occ_flagged
```

## Format

A data frame where each row corresponds to a georeferenced occurrence of
*A. angustifolia*.

## See also

`occurrences`,
[`standardize_countries()`](https://wevertonbio.github.io/RuHere/reference/standardize_countries.md),
[`standardize_states()`](https://wevertonbio.github.io/RuHere/reference/standardize_states.md),
[`flag_florabr()`](https://wevertonbio.github.io/RuHere/reference/flag_florabr.md),
[`flag_wcvp()`](https://wevertonbio.github.io/RuHere/reference/flag_wcvp.md),
[`flag_iucn()`](https://wevertonbio.github.io/RuHere/reference/flag_iucn.md),
[`flag_cultivated()`](https://wevertonbio.github.io/RuHere/reference/flag_cultivated.md),
[`flag_inaturalist()`](https://wevertonbio.github.io/RuHere/reference/flag_inaturalist.md),
[`flag_duplicates()`](https://wevertonbio.github.io/RuHere/reference/flag_duplicates.md),
`mapview_here()`

## Examples

``` r
# First rows
head(occ_flagged)
#> # A tibble: 6 × 28
#>   species       decimalLongitude decimalLatitude states biome endemism inside_br
#>   <chr>                    <dbl>           <dbl> <chr>  <chr> <chr>    <lgl>    
#> 1 Araucaria an…            -49.3           -25.4 MG;MS… Atla… Non-end… NA       
#> 2 Araucaria an…            -42.5           -22.3 MG;MS… Atla… Non-end… NA       
#> 3 Araucaria an…            -51.5           -28.3 MG;MS… Atla… Non-end… NA       
#> 4 Araucaria an…            -54.0           -26.5 MG;MS… Atla… Non-end… NA       
#> 5 Araucaria an…            -51.2           -29.7 MG;MS… Atla… Non-end… NA       
#> 6 Araucaria an…            -52.0           -29.1 MG;MS… Atla… Non-end… NA       
#> # ℹ 21 more variables: inside_state <lgl>, inside_biome <lgl>, record_id <chr>,
#> #   country <chr>, country_suggested <chr>, correct_country <lgl>,
#> #   stateProvince <chr>, state_suggested <chr>, correct_state <lgl>,
#> #   year <dbl>, basisOfRecord <chr>, occurrenceRemarks <chr>, habitat <chr>,
#> #   datasetName <chr>, data_source <chr>, florabr_flag <lgl>, wcvp_flag <lgl>,
#> #   iucn_flag <lgl>, cultivated_flag <lgl>, inaturalist_flag <lgl>,
#> #   duplicated_flag <lgl>

# Count flagged vs. unflagged records
table(occ_flagged$correct_country)
#> 
#> FALSE  TRUE 
#>     6  2610 

```
