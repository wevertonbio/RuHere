# Download the latest version of the Fauna do Brazil (Taxonomic Catalog of the Brazilian Fauna)

This function downloads the Taxonomic Catalog of the Brazilian Fauna
database, which is required for filtering occurrence records using
specialists' information via the
[`flag_faunabr()`](https://wevertonbio.github.io/RuHere/reference/flag_faunabr.md)
function.

## Usage

``` r
faunabr_here(
  data_dir,
  data_version = "latest",
  solve_discrepancy = TRUE,
  overwrite = TRUE,
  remove_files = TRUE,
  verbose = TRUE
)
```

## Arguments

- data_dir:

  (character) a directory to save the data downloaded from Fauna do
  Brazil.

- data_version:

  (character) version of the Fauna do Brazil database to download. Use
  "latest" to get the most recent version, which is updated frequently.
  Alternatively, specify an older version (e.g., data_version="1.2").
  Default value is "latest".

- solve_discrepancy:

  (logical) whether to resolve inconsistencies between species and
  subspecies information. When set to TRUE (default), species
  information is updated based on unique data from subspecies. For
  example, if a subspecies occurs in a certain state, it implies that
  the species also occurs in that state.

- overwrite:

  (logical) If TRUE, data is overwritten. Default is TRUE.

- remove_files:

  (logical) whether to remove the downloaded files used in building the
  final dataset. Default is TRUE.

- verbose:

  (logical) whether to display messages during function execution. Set
  to TRUE to enable display, or FALSE to run silently. Default is TRUE.

## Value

A message indicating that the data were successfully saved in the
directory specified by `data_dir`.

## Examples

``` r
# \donttest{
# Define a directory to save the data
data_dir <- tempdir() # Here, a temporary directory

# Download the latest version of the Flora e Funga do Brazil database
faunabr_here(data_dir = data_dir)
#> Getting data from Taxonomic Catalog of the Brazilian Fauna ...
#> Data will be saved in /tmp/RtmpwXQnG1/faunabr
#> Downloading version: 1.50
#> Merging data. Please wait a moment...
#> Data will be saved in /tmp/RtmpwXQnG1/faunabr
#> Data downloaded and merged successfully. Final data saved in/tmp/RtmpwXQnG1/faunabr/1.50/CompleteBrazilianFauna.gz
#> Data sucessfuly saved in /tmp/RtmpwXQnG1/faunabr
#> 
#> Please don't forget to cite:
#> 
#> Trindade WCF (2025) faunabr: An R package to explore taxonomic data and map species distributions using the Catalogo Taxonomico da Fauna do Brasil. Zoologia 42: e25027. <https://doi.org/10.1590/S1984-4689.v42.e25027>.
# }
```
