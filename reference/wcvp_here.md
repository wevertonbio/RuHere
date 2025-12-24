# Download distribution data from the World Checklist of Vascular Plants (WCVP)

This function downloads the World Checklist of Vascular Plants database,
which is required for filtering occurrence records using specialists'
information via the
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
if (FALSE) { # \dontrun{
# Define a directory to save the data
data_dir <- tempdir() # Here, a temporary directory

# Download the WCVP database
wcvp_here(data_dir = data_dir)
} # }
```
