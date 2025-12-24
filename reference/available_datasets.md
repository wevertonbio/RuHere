# Check the available distribution datasets for a set of species

This function checks which datasets contain distributional information
for a given set of species, based on expert-curated sources. It searches
the selected datasets and reports whether each species has available
distribution data.

## Usage

``` r
available_datasets(
  data_dir,
  species,
  datasets = "all",
  return_distribution = FALSE
)
```

## Arguments

- data_dir:

  (character) directory path where the datasets were saved. See
  *Details* for more information.

- species:

  (character) vector with the species names to be checked for the
  availability of distributional information.

- datasets:

  (character) vector indicating which datasets to search. Options are
  `"all"`, `"florabr"`, `"wcvp"`, `"iucn"`, `"bien"`, and `"faunabr"`.
  Default searches all datasets.

- return_distribution:

  (logical) whether to return the spatial objects (`SpatVector`)
  representing the distribution regions of the species found in the
  selected datasets. Default is `FALSE`.

## Value

If `return_distribution = FALSE`, a data.frame containing the species
names and the datasets where distributional information is available. If
`return_distribution = TRUE`, it also returns a list containing the
`SpatVector` objects representing the species ranges.

## Details

The distribution datasets can be obtained using the functions
[`florabr_here()`](https://wevertonbio.github.io/RuHere/reference/florabr_here.md),
[`wcvp_here()`](https://wevertonbio.github.io/RuHere/reference/wcvp_here.md),
[`bien_here()`](https://wevertonbio.github.io/RuHere/reference/bien_here.md),
and
[`faunabr_here()`](https://wevertonbio.github.io/RuHere/reference/faunabr_here.md),
which download and prepare the corresponding sources for use in
`RuHere`.

## Examples

``` r
# Set directory where datasets were saved
# Here, we'll use the directory where the example datasets are stored
datadir <- system.file("extdata", "datasets",  package = "RuHere")
# Check available datasets
d <- available_datasets(data_dir = datadir,
                        species = c("Araucaria angustifolia",
                                    "Handroanthus serratifolius",
                                    "Cyanocorax caeruleus"))
# Check available datasets and return distribution
d2 <- available_datasets(data_dir = datadir,
                         species = c("Araucaria angustifolia",
                                     "Handroanthus serratifolius",
                                     "Cyanocorax caeruleus"),
                         return_distribution = TRUE)
```
