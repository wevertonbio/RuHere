# Submit a request to download occurrence data from GBIF.

Submit a request to download occurrence data from GBIF.

## Usage

``` r
request_gbif(gbif_info, hasCoordinate = TRUE,
                    hasGeospatialIssue = FALSE, format = "DWCA",
                    gbif_user = NULL, gbif_pwd = NULL, gbif_email = NULL,
                    additional_predicates = NULL)
```

## Arguments

- gbif_info:

  an object of class 'gbif_info' resulted by the
  [`prepare_gbif_download()`](https://wevertonbio.github.io/RuHere/reference/prepare_gbif_download.md)
  function.

- hasCoordinate:

  (logical) whether to retrieve only records with coordinates. Default
  is TRUE.

- hasGeospatialIssue:

  (logical) whether to retrieve records identified with geospatial
  issue. Default is FALSE.

- format:

  (character) the download format. Options available are 'DWCA',
  'SIMPLE_CSV', or 'SPECIES_LIST', Default is DWCA'.

- gbif_user:

  (character) user name within GBIF's website. Default is NULL, meaning
  it will try to obtain this information from the R enviroment. (check
  [`set_gbif_credentials()`](https://wevertonbio.github.io/RuHere/reference/set_gbif_credentials.md))
  for more details.

- gbif_pwd:

  (character) user password within GBIF's website. Default is NULL,
  meaning it will try to obtain this information from the R enviroment.

- gbif_email:

  (character) user email within GBIF's website. Default is NULL, meaning
  it will try to obtain this information from the R enviroment.

- additional_predicates:

  (character or occ_predicate) additional supported predicates that can
  be combined to build more complex download requests. See
  [`rgbif::pred()`](https://docs.ropensci.org/rgbif/reference/download_predicate_dsl.html)
  for details.

## Value

A download request key returned by the GBIF API, which can be used to
monitor or retrieve the download.

## Details

You can use the object returned by this function to check the download
request progress with
[`rgbif::occ_download_wait()`](https://docs.ropensci.org/rgbif/reference/occ_download_wait.html)

## Note

This function requires an active internet connection and valid GBIF
credentials.

## Examples

``` r
if (FALSE) { # \dontrun{
# Prepare data to request GBIF download
gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
# Submit a request to download occurrences
gbif_requested <- request_gbif(gbif_info = gbif_prepared)
# Check progress
rgbif::occ_download_wait(gbif_requested)
} # }
```
