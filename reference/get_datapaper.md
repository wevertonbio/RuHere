# Download and merge biodiversity data papers

Downloads, reads, and merges published biodiversity data papers (see
details for available options). The function allows downloading single
or multiple datasets simultaneously, optionally filtering records by
species name and saving the consolidated table to disk in different file
formats.

## Usage

``` r
get_datapaper(
  datapaper,
  species = NULL,
  save = FALSE,
  dir = NULL,
  file.format = "gz",
  overwrite = FALSE,
  verbose = TRUE
)
```

## Arguments

- datapaper:

  (character) vector with the names of the data papers to download.
  Valid options are: `"atlantic"`, `"brazil road-kill"`, `"dryflor"`,
  `"neotropical xenarthrans"`, and `"neotroptree"`.

- species:

  (character) optional species name or vector of species names to filter
  the dataset. Default is `NULL` (returns all species).

- save:

  (logical) whether to save the merged output file to disk. If `FALSE`,
  files are downloaded to a temporary session directory. Default is
  `FALSE`.

- dir:

  (character) directory path where the output file will be saved if
  `save = TRUE`. Default is `NULL`.

- file.format:

  (character) file format to save the merged table. Valid options are
  `"gz"` (compressed CSV), `"csv"`, or `"rds"`. Default is `"gz"`

- overwrite:

  (logical) whether to overwrite existing downloaded files in the
  destination directory. Default is `FALSE`

- verbose:

  (logical) whether to display messages during function execution. Set
  to TRUE to enable display, or FALSE to run silently. Default is TRUE.

## Value

A `data.table` containing the consolidated records from the selected
data papers.

## Details

Available datasets that can be requested in `datapaper`:

- `"atlantic"`: Atlantic data papers compilations.

- `"brazil road-kill"`: Brazil road-kill dataset.

- `"dryflor"`: Latin American and Caribbean Seasonally Dry Tropical
  Forests (DryFlor) dataset.

- `"neotropical xenarthrans"`: Neotropical Xenarthrans occurrence
  dataset.

- `"neotroptree"`: NeoTropTree tree species checklist database.

## Note

When using any dataset retrieved by this function in publications or
research reports, please cite the original authors. Use the function
[`cite_datapaper()`](https://wevertonbio.github.io/RuHere/reference/cite_datapaper.md)
to obtain the complete bibliographic references.

## Examples

``` r
 if (FALSE) { # \dontrun{
# Download Atlantic dataset
res <- get_datapaper(datapaper = "atlantic")

# Download multiple datasets and filter by a specific species
res_sp <- get_datapaper(
  datapaper = c("atlantic", "dryflor"),
  species = "Araucaria angustifolia")
} # }
```
