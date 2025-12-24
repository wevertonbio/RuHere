# Format and standardize column names and data types of an occurrence dataset

Format and standardize column names and data types of an occurrence
dataset

## Usage

``` r
format_columns(
  occ,
  metadata,
  extract_binomial = TRUE,
  binomial_from = NULL,
  include_subspecies = FALSE,
  include_variety = FALSE,
  check_numeric = TRUE,
  numeric_columns = NULL,
  check_encoding = TRUE,
  data_source = NULL,
  progress_bar = FALSE,
  verbose = FALSE
)
```

## Arguments

- occ:

  (data.frame or data.table) a dataset with occurrence records,
  preferably obtained from
  [`import_gbif()`](https://wevertonbio.github.io/RuHere/reference/import_gbif.md),
  [`get_specieslink()`](https://wevertonbio.github.io/RuHere/reference/get_specieslink.md),
  [`get_bien()`](https://wevertonbio.github.io/RuHere/reference/get_bien.md),
  or
  [`get_idigbio()`](https://wevertonbio.github.io/RuHere/reference/get_idigbio.md).

- metadata:

  (character or data.frame) if a character, one of 'gbif',
  'specieslink', 'bien', or 'idigbio', specifying which metadata
  template to use (the corresponding data frames are available in
  [`RuHere::prepared_metadata`](https://wevertonbio.github.io/RuHere/reference/prepared_metadata.md)).
  If a data.frame is provided, it must have 21 columns (see
  **Details**).

- extract_binomial:

  (logical) whether to create a column with the binomial name of the
  species. If FALSE, it will create a column "species" with the exact
  name stored in the scientificName column. Default is TRUE.

- binomial_from:

  (character) the column name in metadata from which to extract the
  binomial name. Only applicable if `extract_binomial = TRUE`. If
  `metadata` corresponds to one of the predefined sources ('gbif',
  specieslink', 'bien', or 'idigbio'), predefined columns will be used
  automatically. Default is "scientificName".

- include_subspecies:

  (logical) whether to include subspecies in the binomial name. Only
  applicable if `extract_binomial = TRUE`. If TRUE, the function
  includes any infraspecific epithet after the pattern "subsp.". Default
  if FALSE.

- include_variety:

  (logical) whether to include variety in the binomial name. Only
  applicable if `extract_binomial = TRUE`. If TRUE, the function
  includes any infraspecific epithet after the pattern "var.". Default
  if FALSE.

- check_numeric:

  (logical) whether to check and coerce the columns specified in
  `numeric_columns` to numeric type. Default is TRUE.

- numeric_columns:

  (character) a vector of column names that must be numeric. Default is
  NULL, meaning that if `check_numeric = TRUE`, the following columns
  will be coerced: 'decimalLongitude', 'decimalLatitude',
  'coordinateUncertaintyInMeters', 'elevation', and 'year'.

- check_encoding:

  (logical) whether to check and fix the encoding of columns that
  typically contain special characters (see **Details**). Default is
  TRUE.

- data_source:

  (character) the source of the occurrence records. Default is NULL,
  meaning it will use the same string provided in `metadata`. If
  `metadata` is a user-defined data.frame, this argument must be
  specified.

- progress_bar:

  (logical) whether to display a progress bar during processing. If
  TRUE, the 'pbapply' package must be installed. Default is `FALSE`.

- verbose:

  (logical) whether to print messages about the progress. Default is
  FALSE.

## Value

A data.frame with standardized column names and data types according to
the specified metadata.

## Details

If a user-defined metadata data.frame is provided, it must include the
following 21 columns: 'scientificName', 'collectionCode',
'catalogNumber', 'decimalLongitude', 'decimalLatitude',
'coordinateUncertaintyInMeters', 'elevation', 'country',
'stateProvince', 'municipality', 'locality', 'year', 'eventDate',
'recordedBy', 'identifiedBy', 'basisOfRecord', 'occurrenceRemarks',
'habitat', 'datasetName', 'datasetKey', and 'key'.

If `check_encoding = TRUE`, the function will inspect and, if necessary,
fix the encoding of these columns: 'collectionCode', 'catalogNumber',
'country', 'stateProvince', municipality', 'locality',
'eventDate','recordedBy', 'identifiedBy', 'basisOfRecord', and
'datasetName'.

## Examples

``` r
# Example with GBIF
data("occ_gbif", package = "RuHere") #Import data example
gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
# Example with SpeciesLink
data("occ_splink", package = "RuHere") #Import data example
splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
# Example with BIEN
data("occ_bien", package = "RuHere") #Import data example
bien_standardized <- format_columns(occ_bien, metadata = "bien")
# Example with idigbio
data("occ_idig", package = "RuHere") #Import data example
idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
```
