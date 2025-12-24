# Download species distribution information from BIEN

This function downloads distribution information from the BIEN database,
required for filtering occurrence records using specialists' information
via the
[`flag_bien()`](https://wevertonbio.github.io/RuHere/reference/flag_bien.md)
function.

## Usage

``` r
bien_here(
  data_dir,
  species,
  synonyms = NULL,
  overwrite = TRUE,
  progress_bar = FALSE,
  verbose = TRUE
)
```

## Arguments

- data_dir:

  (character) directory to save the data downloaded from BIEN.

- species:

  (character) a vector of species names for which to retrieve
  distribution information.

- synonyms:

  (data.frame) an optional data.frame containing synonyms of the target
  species. The first column must contain the target species names, and
  the second column their corresponding synonyms. Default is `NULL`. See
  details for more information.

- overwrite:

  (logical) whether to overwrite existing files. Default is `TRUE`.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) whether to display progress messages. Default is `TRUE`.

## Value

A data frame indicating whether the polygon(s) representing the species
range are available in BIEN. If the range is available, a GeoPackage
file (.gpkg) is saved in `data_dir/bien`. The file name corresponds to
the species name, with an underscore (“\_”) replacing the space between
the genus and the specific epithet.

## Details

This function uses the
[`BIEN::BIEN_ranges_load_species()`](https://rdrr.io/pkg/BIEN/man/BIEN_ranges_load_species.html)
function to retrieve polygons representing the distribution ranges of
species available in the BIEN database.

Because taxonomic information in BIEN may be outdated, you can
optionally provide a table of synonyms to broaden the search. The
synonyms data.frame should have the accepted species in the first column
and their synonyms in the second. See `RuHere::synonys` for an example.

## Examples

``` r
# Define a directory to save the data
data_dir <- tempdir() # Here, a temporary directory

# Download species distribution information from BIEN
bien_here(data_dir = data_dir, species = "Handroanthus serratifolius")
#>                       species range_available
#>                        <char>          <lgcl>
#> 1: Handroanthus serratifolius            TRUE
```
