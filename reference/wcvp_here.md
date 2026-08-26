# Download distribution data from the World Checklist of Vascular Plants (WCVP)

This function downloads the World Checklist of Vascular Plants database,
which is required for filtering occurrence records using specialists'
range information via the
[`flag_wcvp()`](https://wevertonbio.github.io/RuHere/reference/flag_wcvp.md)
function.

## Usage

``` r
wcvp_here(
  data_dir,
  overwrite = TRUE,
  remove_files = TRUE,
  timeout = 300,
  verbose = TRUE
)
```

## Arguments

- data_dir:

  (character) a directory to save the data downloaded from WCVP.

- overwrite:

  (logical) If TRUE, data is overwritten. Default is TRUE.

- remove_files:

  (logical) whether to remove the downloaded files used in building the
  final dataset. Default is TRUE.

- timeout:

  (numeric) maximum time (in seconds) allowed for downloading. Default
  is 300. Slower internet connections may require higher values.

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

# Download the WCVP database
wcvp_here(data_dir = data_dir)
#> Task 1 of 3: Downloading data from the World Checklist of Vascular Plants (WCVP) repository...
#> Task 2 of 3: Merging data...
#> Task 3 of 3: Downloading map from the World Geographical Scheme for Recording Plant Distributions (WGSRPD)...
#> Warning: downloaded length 0 != reported length 92
#> Warning: cannot open URL 'https://zenodo.org/records/17455838/files/wgsrpd.gpkg?download=1': HTTP status was '504 Gateway Timeout'
#> Error in utils::download.file(url = "https://zenodo.org/records/17455838/files/wgsrpd.gpkg?download=1",     destfile = file.path(data_dir, "wgsrpd", "wgsrpd.gpkg"),     method = "auto", mode = "wb", cacheOK = TRUE, quiet = quiet): cannot open URL 'https://zenodo.org/records/17455838/files/wgsrpd.gpkg?download=1'
# }
```
