# Bind occurrences after standardizing columns

Combines multiple occurrence data frames (for example, from GBIF,
SpeciesLink, BIEN, or iDigBio) into a single standardized dataset. This
is particularly useful after using
[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)
to ensure column compatibility across data sources.

## Usage

``` r
bind_here(..., fill = FALSE)
```

## Arguments

- ...:

  (data.frame) two or more data frames with occurrence records to
  combine.

- fill:

  (logical) whether to fills missing columns with `NA`. Default is
  FALSE.

## Value

A `data.frame` containing all occurrence records combined.

## Details

When `fill = TRUE`, columns not shared among the input data frames are
added and filled with `NA`, ensuring that all columns align before
binding. Internally, this function uses
[`data.table::rbindlist()`](https://rdrr.io/pkg/data.table/man/rbindlist.html)
for efficient row binding.

## Examples

``` r
# Import and standardize GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
# Import and standardize SpeciesLink
data("occ_splink", package = "RuHere") #Import data example
splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
# Import and standardize BIEN
data("occ_bien", package = "RuHere") #Import data example
bien_standardized <- format_columns(occ_bien, metadata = "bien")
# Import and standardize idigbio
data("occ_idig", package = "RuHere") #Import data example
idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
# Merge all
all_occ <- bind_here(gbif_standardized, splink_standardized,
                      bien_standardized, idig_standardized)
```
