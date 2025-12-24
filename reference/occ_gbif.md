# Occurrence records of *Araucaria angustifolia* from GBIF

A cleaned dataset of occurrence records for *Araucaria angustifolia*
(Parana pine) retrieved from GBIF.

Records were downloaded using the package’s GBIF workflow
([`prepare_gbif_download()`](https://wevertonbio.github.io/RuHere/reference/prepare_gbif_download.md),
[`request_gbif()`](https://wevertonbio.github.io/RuHere/reference/request_gbif.md),
[`import_gbif()`](https://wevertonbio.github.io/RuHere/reference/import_gbif.md)),
and then cleaned using the internal flagging workflow (duplicate
detection and removal).

## Usage

``` r
occ_gbif
```

## Format

A data frame containing georeferenced GBIF occurrence records for *A.
angustifolia* after all cleaning steps.

## See also

[`prepare_gbif_download()`](https://wevertonbio.github.io/RuHere/reference/prepare_gbif_download.md),
[`request_gbif()`](https://wevertonbio.github.io/RuHere/reference/request_gbif.md),
[`import_gbif()`](https://wevertonbio.github.io/RuHere/reference/import_gbif.md),
[`flag_duplicates()`](https://wevertonbio.github.io/RuHere/reference/flag_duplicates.md),
[`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md)

## Examples

``` r
# Preview dataset
head(occ_gbif)
#> # A tibble: 6 × 21
#>   acceptedScientificName           collectionCode catalogNumber decimalLongitude
#>   <chr>                            <chr>          <chr>                    <dbl>
#> 1 Araucaria angustifolia (Bertol.… ALTA-VP        74703                    -52.8
#> 2 Araucaria angustifolia (Bertol.… HCF            43511                    -52.3
#> 3 Araucaria angustifolia (Bertol.… Observations   326380478                -48.0
#> 4 Araucaria angustifolia (Bertol.… Observations   327335877                -49.2
#> 5 Araucaria angustifolia (Bertol.… Observations   326095249                -49.0
#> 6 Araucaria angustifolia (Bertol.… Observations   326823098                -49.0
#> # ℹ 17 more variables: decimalLatitude <dbl>,
#> #   coordinateUncertaintyInMeters <dbl>, elevation <dbl>, countryCode <chr>,
#> #   stateProvince <chr>, municipality <chr>, locality <chr>, year <int>,
#> #   eventDate <chr>, recordedBy <chr>, identifiedBy <chr>, basisOfRecord <chr>,
#> #   occurrenceRemarks <chr>, habitat <chr>, datasetName <chr>,
#> #   datasetKey <chr>, speciesKey <int>

# Number of cleaned records
nrow(occ_gbif)
#> [1] 2989
```
