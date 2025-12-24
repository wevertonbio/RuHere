# Prepare data to request GBIF download

Prepare data to request GBIF download

## Usage

``` r
prepare_gbif_download(
  species,
  rank = NULL,
  kingdom = NULL,
  phylum = NULL,
  class = NULL,
  order = NULL,
  family = NULL,
  genus = NULL,
  strict = FALSE,
  progress_bar = FALSE,
  ...
)
```

## Arguments

- species:

  (character) a vector of species name(s).

- rank:

  (character) optional taxonomic rank (for example, 'species' or
  'genus'). Default is NULL, meaning it will return species matched
  across all ranks.

- kingdom:

  (character) optional taxonomic kingdom (for example, 'Plantae' or
  'Animalia'). Default is NULL, meaning it will return species matched
  across all kingdoms.

- phylum:

  (character) optional taxonomic phylum. Default is NULL, meaning it
  will return species matched across all phyla.

- class:

  (character) optional taxonomic class. Defaults is NULL, meaning it
  will return species matched across all classes.

- order:

  (character) optional taxonomic order. Defaults is NULL, meaning it
  will return species matched across all orders

- family:

  (character) optional taxonomic family. Defaults is NULL, meaning it
  will return species matched across all families.

- genus:

  (character) optional taxonomic genus. Defaults is NULL, meaning it
  will return species matched across all genus.

- strict:

  (logical) If TRUE, it (fuzzy) matches only the given name, but never a
  taxon in the upper classification. Default is FALSE.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- ...:

  other parameters passed to
  [`rgbif::occ_count()`](https://docs.ropensci.org/rgbif/reference/occ_count.html).

## Value

A data.frame with species information, including the number of
occurrences and other related details.

## Note

This function requires an active internet connection to access GBIF
data.

## Examples

``` r
if (FALSE) { # \dontrun{
gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
} # }
```
