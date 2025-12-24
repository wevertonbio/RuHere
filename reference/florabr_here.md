# Download the latest version of Flora e Funga do Brasil database

This function downloads the Flora e Funga do Brasil database, which is
required for filtering occurrence records using specialists' information
via the
[`flag_florabr()`](https://wevertonbio.github.io/RuHere/reference/flag_florabr.md)
function.

## Usage

``` r
florabr_here(
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

  (character) a directory to save the data downloaded from Flora e Funga
  do Brasil.

- data_version:

  (character) version of the Flora e Funga do Brasil database to
  download. Use "latest" to get the most recent version, updated weekly.
  Alternatively, specify an older version (e.g.,
  data_version="393.319"). Default value is "latest".

- solve_discrepancy:

  (logical) whether to resolve discrepancies between species and
  subspecies/varieties information. When set to TRUE, species
  information is updated based on unique data from varieties and
  subspecies. For example, if a subspecies occurs in a certain biome, it
  implies that the species also occurs in that biome. Default is TRUE.

- overwrite:

  (logical) if TRUE, data is overwritten. Default = TRUE.

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

# Download the latest version of the Flora e Funga do Brasil database
florabr_here(data_dir = data_dir)
} # }
```
