# get_idigbio

Downloads species occurrence records from the iDigBio (Integrated
Digitized Biocollections) database with flexible taxonomic and
geographic filtering options.

## Usage

``` r
get_idigbio(species = NULL, fields = "all", genus = NULL,
family = NULL, order = NULL, phylum = NULL, kingdom = NULL, country = NULL,
county = NULL, limit = NULL, offset = NULL, dir, filename = "idigbio_output",
save = FALSE, compress = FALSE, file.format = "csv", verbose = TRUE, ...)
```

## Arguments

- species:

  (character) scientific name(s) of species to search for. Default is
  `NULL`.

- fields:

  (character) fields to retrieve from iDigBio. Default is `"all"`.

- genus:

  (character) genus name for filtering results. Default is `NULL`.

- family:

  (character) family name for filtering results. Default is `NULL`.

- order:

  (character) order name for filtering results. Default is `NULL`.

- phylum:

  (character) phylum name for filtering results. Default is `NULL`.

- kingdom:

  (character) kingdom name for filtering results. Default is `NULL`.

- country:

  (character) country name for geographic filtering. Default is `NULL`.

- county:

  (character) county name for geographic filtering. Default is `NULL`.

- limit:

  (numeric) maximum number of records to retrieve. Default is `NULL` (no
  limit).

- offset:

  (numeric) number of records to skip before starting retrieval. Default
  is `NULL` (starts at 0).

- dir:

  (character) directory path where the file will be saved. Required if
  `save = TRUE`.

- filename:

  (character) name of the output file without extension. Default is
  `"idigbio_output"`.

- save:

  (logical) if `TRUE`, saves the results to a CSV file. Default is
  `FALSE`.

- compress:

  (logical) if `TRUE` and `save = TRUE`, compresses the output file as
  .csv.zip. Default is `FALSE`.

- file.format:

  (character) file format for saving output (`"csv"`, `"rds"`). Default
  is `"csv"`

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

- ...:

  additional arguments passed to
  [`ridigbio::idig_search_records()`](https://idigbio.github.io/ridigbio/reference/idig_search_records.html).

## Value

A `data.frame` containing occurrence records from iDigBio with the
requested fields.

## Examples

``` r
if (FALSE) { # \dontrun{
## search for a single species
records_basic <- get_idigbio(species = "Arecaceae")

## search for multiple species
records_multiple <- get_idigbio(
  species = c("Araucaria angustifolia"),
  limit = 100)

## save results as a compressed RDS file
records_saved_rds <- get_idigbio(
  species = "Anacardiaceae",
  limit = 50,
  dir = tempdir(),
  filename = "anacardiaceae_records",
  save = TRUE,
  compress = TRUE,
  file.format = "rds")
} # }
```
