# Download species distribution information from IUCN

This function downloads information on species distributions from the
IUCN Red List, required for filtering occurrence records using
specialists' information via the
[`flag_iucn()`](https://wevertonbio.github.io/RuHere/reference/flag_iucn.md)
function.

## Usage

``` r
iucn_here(
  data_dir,
  species,
  synonyms = NULL,
  iucn_credential = NULL,
  overwrite = FALSE,
  progress_bar = FALSE,
  verbose = FALSE,
  return_data = TRUE
)
```

## Arguments

- data_dir:

  (character) directory to save the data downloaded from IUCN.

- species:

  (character) a vector of species names for which to retrieve
  distribution information.

- synonyms:

  (data.frame) an optional data.frame containing synonyms of the target
  species. The first column must contain the target species names, and
  the second column their corresponding synonyms. Default is `NULL`. See
  details for more information.

- iucn_credential:

  (character) your IUCN API key. Default is `NULL`, in which case the
  function will attempt to read the API key from your R environment. You
  can set it in advance using the
  [`set_iucn_credentials()`](https://wevertonbio.github.io/RuHere/reference/set_iucn_credentials.md)
  function.

- overwrite:

  (logical) whether to overwrite existing files. Default is `FALSE`.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) whether to display progress messages. Default is `FALSE`.

- return_data:

  (logical) whether to return a data frame containing the species
  distribution information downloaded from IUCN. Default is `TRUE`.

## Value

A message indicating that the data were successfully saved in the
directory specified by `data_dir`. If `return_data = TRUE`, the function
additionally returns a data frame containing the species distribution
information retrieved from IUCN.

## Details

This function uses the
[`rredlist::rl_species()`](https://docs.ropensci.org/rredlist/reference/rl_species.html)
function to retrieve distribution data from the IUCN Red List. The data
include information at the country and regional levels, following the
World Geographical Scheme for Recording Plant Distributions (WGSRPD) —
but applicable to both plants and animals.

Unfortunately, the range polygons available at
<https://www.iucnredlist.org/resources/spatial-data-download> cannot be
accessed automatically.

Because taxonomic information in IUCN may be outdated, you can
optionally provide a table of synonyms to broaden the search. The
synonyms data.frame should have the accepted species in the first column
and their synonyms in the second. See `RuHere::synonys` for an example.

The function also downloads the WGSRPD map used to represent
distribution regions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define a directory to save the data
data_dir <- tempdir() # Here, a temporary directory

# Download species distribution information from IUCN
iucn_here(data_dir = data_dir, species = "Araucaria angustifolia")
} # }
```
