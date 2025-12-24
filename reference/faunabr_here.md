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
if (FALSE) { # \dontrun{
# Define a directory to save the data
data_dir <- tempdir() # Here, a temporary directory

# Download the latest version of the Flora e Funga do Brazil database
faunabr_here(data_dir = data_dir)
} # }
```
