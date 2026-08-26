# Download a list of species recorded inside a spatial polygon from GBIF.

Download a list of species recorded inside a spatial polygon from GBIF,
with support for optional higher-taxon filtering.

## Usage

``` r
request_gbif_specieslist(
  spatial_polygon,
  kingdom = NULL,
  phylum = NULL,
  class = NULL,
  order = NULL,
  family = NULL,
  genus = NULL,
  species = NULL,
  tolerance = 0.01,
  gbif_user = NULL,
  gbif_pwd = NULL,
  gbif_email = NULL,
  verbose = TRUE
)
```

## Arguments

- spatial_polygon:

  an object of class `SpatVector` representing the area of interest.

- kingdom, phylum, class, order, family, genus, species:

  (character) optional taxonomic filters. Default is NULL.

- tolerance:

  (numeric) tolerance in degrees for geometry simplification. Default is
  0.01.

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

- verbose:

  (logical) if TRUE, prints messages about the progress. Default is
  `FALSE`.

## Value

A download request key returned by the GBIF API, which can be used to
monitor or retrieve the download of the species list.

## Details

This function converts the input `SpatVector` polygon into Well-Known
Text (WKT) format and submits an asynchronous query using GBIF's
`SPECIES_LIST` download format.

You can use the object returned by this function to check the download
request progress with
[`rgbif::occ_download_wait()`](https://docs.ropensci.org/rgbif/reference/occ_download_wait.html)

## Note

This function requires an active internet connection and valid GBIF
credentials. Set them in advance using
[`set_gbif_credentials()`](https://wevertonbio.github.io/RuHere/reference/set_gbif_credentials.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Set your GBIF credentials (required before running this function)
# set_gbif_credentials(gbif_username = "your_username",
#                       gbif_email = "your_email@example.com",
#                       gbif_password = "your_password")

# Create a sample polygon
coords <- matrix(c(-48, -16, -47, -16, -47, -15, -48, -15, -48, -16),
                 ncol = 2, byrow = TRUE)
poly <- vect(coords, type = "polygons", crs = "EPSG:4326")

# Submit a request to download list of Malvaceae species in the area
gbif_requested_sl <- request_gbif_specieslist(spatial_polygon = poly,
                                            family = "Malvaceae")
# Monitor download progress
rgbif::occ_download_wait(gbif_requested_sl)

# Import completed species list
sl <- import_gbif(gbif_requested_sl, select_columns = FALSE)
} # }
```
