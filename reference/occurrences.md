# Integrated occurrence dataset for three example species

A harmonized, multi-source occurrence dataset containing cleaned
georeferenced records for three species:

- *Araucaria angustifolia* (Parana pine)

- *Cyanocorax caeruleus* (Azure jay)

- *Handroanthus serratifolius* (Yellow trumpet tree)

Records were retrieved from **GBIF**, **speciesLink**, **BIEN**, and
**iDigBio**, standardized through the package workflow, merged, and
cleaned to remove duplicates.

## Usage

``` r
occurrences
```

## Format

A data frame where each row represents a georeferenced occurrence record
for one of the three species.

Columns correspond to the standardized output of
[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md),
including:

- `species`: Cleaned binomial species name

- `decimalLongitude`, `decimalLatitude`: Coordinates

- `year`: Year of collection/observation

- Various taxonomic, temporal, locality, and metadata fields

- Source identifiers added by
  [`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)
  (e.g., `data_source`)

## See also

[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md),
[`bind_here()`](https://wevertonbio.github.io/RuHere/reference/bind_here.md),
[`flag_duplicates()`](https://wevertonbio.github.io/RuHere/reference/flag_duplicates.md),
[`remove_flagged()`](https://wevertonbio.github.io/RuHere/reference/remove_flagged.md)

## Examples

``` r
# Show the first rows
head(occurrences)
#>   record_id                species catalogNumber decimalLongitude
#> 1   gbif_83 Araucaria angustifolia     323502165        -49.26743
#> 2   gbif_85 Araucaria angustifolia     325575408        -42.49838
#> 3   gbif_86 Araucaria angustifolia     325217148        -42.53397
#> 4   gbif_90 Araucaria angustifolia     322837754        -49.66112
#> 5   gbif_91 Araucaria angustifolia     322601546        -46.68649
#> 6   gbif_92 Araucaria angustifolia     276301823        -49.38901
#>   decimalLatitude country  stateProvince locality year     basisOfRecord
#> 1       -25.42535      BR         Paraná          2025 HUMAN_OBSERVATION
#> 2       -22.30930      BR Rio de Janeiro          2025 HUMAN_OBSERVATION
#> 3       -22.06159      BR Rio de Janeiro          2025 HUMAN_OBSERVATION
#> 4       -27.18439      BR Santa Catarina          2025 HUMAN_OBSERVATION
#> 5       -23.33256      BR      São Paulo          2025 HUMAN_OBSERVATION
#> 6       -25.59535      BR         Paraná          2025 HUMAN_OBSERVATION
#>   occurrenceRemarks habitat                             datasetName data_source
#> 1                           iNaturalist research-grade observations        gbif
#> 2                           iNaturalist research-grade observations        gbif
#> 3                           iNaturalist research-grade observations        gbif
#> 4                           iNaturalist research-grade observations        gbif
#> 5                           iNaturalist research-grade observations        gbif
#> 6                           iNaturalist research-grade observations        gbif

# Number of occurrences per species
table(occurrences$species)
#> 
#>     Araucaria angustifolia       Cyanocorax caeruleus 
#>                        924                       1035 
#> Handroanthus serratifolius 
#>                       2121 
```
