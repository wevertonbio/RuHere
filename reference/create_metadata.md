# Create metadata template

This function creates a metadata template to be used in
[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)
for formatting and standardizing column names and classes in occurrence
datasets. All column names specified as arguments must be present in the
`occ` dataset.

If you obtained data from GBIF, SpeciesLink, BIEN or iDigBio using the
functions provided in the RuHere package, you do not need to use this
function, as the package already includes metadata templates for these
datasets.

## Usage

``` r
create_metadata(
  occ,
  scientificName,
  decimalLongitude,
  decimalLatitude,
  collectionCode = NA,
  catalogNumber = NA,
  coordinateUncertaintyInMeters = NA,
  elevation = NA,
  country = NA,
  stateProvince = NA,
  municipality = NA,
  locality = NA,
  year = NA,
  eventDate = NA,
  recordedBy = NA,
  identifiedBy = NA,
  basisOfRecord = NA,
  occurrenceRemarks = NA,
  habitat = NA,
  datasetName = NA,
  datasetKey = NA,
  key = NA
)
```

## Arguments

- occ:

  (data.frame or data.table) a dataset with occurrence records to be
  standardized.

- scientificName:

  (character) column name in `occ` with the scientific name of the
  species.

- decimalLongitude:

  (character) column name in `occ` with the longitude.

- decimalLatitude:

  (character) column name in `occ` with the latitude.

- collectionCode:

  (character) an optional column name in `occ` with the collection code.

- catalogNumber:

  (character) an optional column name in `occ` with the catalog number.

- coordinateUncertaintyInMeters:

  (character) an optional column name with the coordinate uncertainty in
  meters.

- elevation:

  (character) an optional column name with the elevation information.

- country:

  (character) an optional column name with the country of the record.

- stateProvince:

  (character) an optional column name with the state or province of the
  record.

- municipality:

  (character) an optional column name with the municipality of the
  record.

- locality:

  (character) an optional column name with the locality description.

- year:

  (character) an optional column name with the year when the occurrence
  was recorded.

- eventDate:

  (character) an optional column name with the event date.

- recordedBy:

  (character) an optional column name with the name of the collector or
  recorder.

- identifiedBy:

  (character) an optional column name with the name of the identifier.

- basisOfRecord:

  (character) an optional column name with the basis of record.

- occurrenceRemarks:

  (character) an optional column name with remarks about the occurrence.

- habitat:

  (character) an optional column name with the habitat description.

- datasetName:

  (character) an optional column name with the dataset name.

- datasetKey:

  (character) an optional column name with the dataset key.

- key:

  (character) an optional column name with the unique occurrence
  identifier.

## Value

A `data.frame` containing a metadata template that can be directly used
in the
[`format_columns()`](https://wevertonbio.github.io/RuHere/reference/format_columns.md)
function.

## Examples

``` r
# Load data example
# Occurrences of Puma concolor from the atlanticr R package
data("puma_atlanticr", package = "RuHere")
# Create metadata to standardize the occurrences
puma_metadata <- create_metadata(occ = puma_atlanticr,
                                 scientificName = "actual_species_name",
                                 decimalLongitude = "longitude",
                                 decimalLatitude = "latitude",
                                 elevation = "altitude",
                                 country = "country",
                                 stateProvince = "state",
                                 municipality = "municipality",
                                 locality = "study_location",
                                 year = "year_finish",
                                 habitat = "vegetation_type",
                                 datasetName = "reference")
# Now, we can use this metadata to standardize the columns
puma_occ <- format_columns(occ = puma_atlanticr, metadata = puma_metadata,
                           binomial_from = "actual_species_name",
                           data_source = "atlanticr")
#> Warning: NAs introduced by coercion
```
