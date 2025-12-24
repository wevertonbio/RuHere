# Metadata templates used internally by `format_columns()`

A named list of data frames containing metadata templates for the main
biodiversity data providers supported by the package (GBIF, SpeciesLink,
iDigBio, and BIEN).

These templates are used internally by
[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)
to harmonize columns.

## Usage

``` r
prepared_metadata
```

## Format

A named list of four data frames:

- **`$gbif`** — template for GBIF dataset.

- **`$specieslink`** — template for SpeciesLink dataset.

- **`$idigbio`** — template for iDigBio dataset.

- **`$bien`** — template for BIEN dataset.

## Details

Each element of `prepared_metadata` is a single-row data frame where:

- **column names** correspond to the package’s standardized output
  fields

- **values in the row** represent the original column names used by each
  data provider

These mappings allow
[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)
to:

- rename fields (e.g., `scientificname` → `scientificName`)

- identify which variables are missing or provider-specific

- coerce classes consistently (e.g., dates, coordinates)

- ensure compatibility when combining datasets from different sources

## See also

[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)

## Examples

``` r
# View template for GBIF records
prepared_metadata$gbif
#>           scientificName collectionCode catalogNumber decimalLongitude
#> 1 acceptedScientificName collectionCode catalogNumber decimalLongitude
#>   decimalLatitude coordinateUncertaintyInMeters elevation     country
#> 1 decimalLatitude coordinateUncertaintyInMeters elevation countryCode
#>   stateProvince municipality locality year eventDate recordedBy identifiedBy
#> 1 stateProvince municipality locality year eventDate recordedBy identifiedBy
#>   basisOfRecord occurrenceRemarks habitat datasetName datasetKey        key
#> 1 basisOfRecord occurrenceRemarks habitat datasetName datasetKey speciesKey

```
