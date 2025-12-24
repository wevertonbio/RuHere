# Download occurrence records from SpeciesLink

Retrieves occurrence data from the
[speciesLink](https://specieslink.net/) network using user-defined
filters. The function allows querying by taxonomic, geographic, and
collection-related parameters.

## Usage

``` r
get_specieslink(species = NULL, key = NULL, dir,
                       filename = "specieslink_output",save = FALSE,
                       basisOfRecord = NULL, family = NULL, institutionCode = NULL,
                       collectionID = NULL, catalogNumber = NULL,
                       kingdom = NULL, phylum = NULL, class = NULL,
                       order = NULL, genus = NULL, specificEpithet = NULL,
                       infraspecificEpithet = NULL, collectionCode = NULL,
                       identifiedBy = NULL, yearIdentified = NULL,
                       country = NULL, stateProvince = NULL, county = NULL,
                       typeStatus = NULL, recordedBy = NULL,
                       recordNumber = NULL, yearCollected = NULL,
                       locality = NULL, occurrenceRemarks = NULL,
                       barcode = NULL, bbox = NULL, landuse_1 = NULL,
                       landuse_year_1 = NULL, landuse_2 = NULL,
                       landuse_year_2 = NULL, phonetic = FALSE,
                       coordinates = NULL, scope = NULL, synonyms = NULL,
                       typus = FALSE, images = FALSE, redlist = NULL,
                       limit = NULL, file.format = "csv",
                       compress = FALSE, verbose = TRUE)
```

## Arguments

- species:

  (character) species name. Default is `NULL`.

- key:

  (character) API key or authentication token if required. Default is
  `NULL`.

- dir:

  (character) directory where files will be saved (if `save = TRUE`).

- filename:

  (character) name of the output file without extension. Default is
  `"specieslink_output"`.

- save:

  (logical) whether to save the results to file. Default is `FALSE`.

- basisOfRecord:

  (character) filter by basis of record. Default is `NULL`.

- family:

  (character) family name. Default is `NULL`.

- institutionCode:

  (character) code of the institution that holds the specimen. Default
  is `NULL`.

- collectionID:

  (character) unique identifier for the collection. Default is `NULL`.

- catalogNumber:

  (character) catalog number of the specimen or record. Default is
  `NULL`.

- kingdom:

  (character) kingdom name. Default is `NULL`.

- phylum:

  (character) phylum name. Default is `NULL`.

- class:

  (character) class name. Default is `NULL`.

- order:

  (character) order name. Default is `NULL`.

- genus:

  (character) genus name. Default is `NULL`.

- specificEpithet:

  (character) specific epithet of the species. Default is `NULL`.

- infraspecificEpithet:

  (character) infraspecific epithet. Default is `NULL`.

- collectionCode:

  (character) code identifying the collection within an institution.
  Default is `NULL`.

- identifiedBy:

  (character) name of the person who identified the specimen. Default is
  `NULL`.

- yearIdentified:

  (numeric) year of identification. Default is `NULL`.

- country:

  (character) country name. Default is `NULL`.

- stateProvince:

  (character) state or province name. Default is `NULL`.

- county:

  (character) county or municipality name. Default is `NULL`.

- typeStatus:

  (character) type status. Default is `NULL`.

- recordedBy:

  (character) collector name. Default is `NULL`.

- recordNumber:

  (numeric) collector’s record number. Default is `NULL`.

- yearCollected:

  (numeric) year of collection. Default is `NULL`.

- locality:

  (character) locality description. Default is `NULL`.

- occurrenceRemarks:

  (character) text field for remarks about the occurrence. Default is
  `NULL`.

- barcode:

  (character) barcode or unique specimen identifier. Default is `NULL`.

- bbox:

  (character) bounding box coordinates in the format
  `"lon_min+lat_min+lon_max+lat_max"`. Default is `NULL`.

- landuse_1:

  (character) land use category for the first year. Default is `NULL`.

- landuse_year_1:

  (numeric) year corresponding to `landuse_1`. Default is `NULL`.

- landuse_2:

  (character) land use category for the second year. Default is `NULL`.

- landuse_year_2:

  (numeric) year corresponding to `landuse_2`. Default is `NULL`.

- phonetic:

  (logical) whether to use phonetic matching for taxon names. Default is
  `FALSE`.

- coordinates:

  (character) whether to include only records with geographic
  coordinates (`"yes"`, `"no"`, `"original"`, `"automatic"`,
  `"blocked"`, `"consistent"`, `"suspect"`)). Default is `NULL`.

- scope:

  (character) scope of the query (`"p"`, `"a"`, `"m"`, `"f"`, `"b"`).
  Default is `NULL`.

- synonyms:

  (chacarter) whether to include synonyms of the specified taxon
  (`"sp2000"`, `"flora2020"`, `"MycoBank"`, `"algaebase"`, `"DSMZ"`,
  `"moure"`). Default is `NULL`.

- typus:

  (logical) whether to filter only type specimens. Default is `FALSE`.

- images:

  (logical) whether to restrict to records with associated images.
  Default is `FALSE`.

- redlist:

  (character) filter by IUCN Red List category. Default is `NULL`.

- limit:

  (numeric) maximum number of records to return. Default is `NULL`.

- file.format:

  (character) file format for saving output (`"csv"`, `"rds"`). Default
  is `"csv"`.

- compress:

  (logical) whether to compress the output file into `.zip`. Default is
  `FALSE`.

- verbose:

  (logical) if `TRUE`, prints messages about the progress and the number
  of species being checked. Default is `TRUE`.

  \#' @details The speciesLink API key can be set permanently using:

      set_specieslink_credentials("your_api_key")

## Value

A `data.frame` containing the occurrence data fields returned by
speciesLink.

## Examples

``` r
if (FALSE) { # \dontrun{
# Retrieve records for Arecaceae in São Paulo
res <- get_specieslink(
  family = "Arecaceae",
  country = "Brazil",
  stateProvince = "São Paulo",
  basisOfRecord = "PreservedSpecimen",
  limit = 10
)

# Save results as compressed CSV
get_specieslink(
  family = "Arecaceae",
  country = "Brazil",
  save = TRUE,
  dir = "data/",
  filename = "arecaceae_sp",
  compress = TRUE
)
} # }
```
