#' Create metadata template
#'
#' @description
#' This function creates a metadata template to be used in `format_columns()`
#' for formatting and standardizing column names and classes in occurrence
#' datasets.
#' All column names specified as arguments must be present in the `occ` dataset.
#'
#' If you obtained data from GBIF, SpeciesLink, BIEN or iDigBio using the
#' functions provided in the RuHere package, you do not need to use this
#' function, as the package already includes metadata templates for these
#' datasets.
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records to be
#' standardized.
#' @param scientificName (character) column name in `occ` with the scientific
#' name of the species.
#' @param decimalLongitude (character) column name in `occ` with the longitude.
#' @param decimalLatitude (character) column name in `occ` with the latitude.
#' @param collectionCode (character) an optional column name in `occ` with the
#' collection code.
#' @param catalogNumber (character) an optional column name in `occ` with the
#' catalog number.
#' @param coordinateUncertaintyInMeters (character) an optional column name with
#' the coordinate uncertainty in meters.
#' @param elevation (character) an optional column name with the elevation
#' information.
#' @param country (character) an optional column name with the country of the
#' record.
#' @param stateProvince (character) an optional column name with the state or
#' province of the record.
#' @param municipality (character) an optional column name with the municipality
#' of the record.
#' @param locality (character) an optional column name with the locality
#' description.
#' @param year (character) an optional column name with the year when the
#' occurrence was recorded.
#' @param eventDate (character) an optional column name with the event date.
#' @param recordedBy (character) an optional column name with the name of the
#' collector or recorder.
#' @param identifiedBy (character) an optional column name with the name of the
#' identifier.
#' @param basisOfRecord (character) an optional column name with the basis of
#' record.
#' @param occurrenceRemarks (character) an optional column name with remarks
#' about the occurrence.
#' @param habitat (character) an optional column name with the habitat
#' description.
#' @param datasetName (character) an optional column name with the dataset name.
#' @param datasetKey (character) an optional column name with the dataset key.
#' @param key (character) an optional column name with the unique occurrence
#' identifier.
#'
#' @returns
#' A `data.frame` containing a metadata template that can be directly used in
#' the `format_columns()` function.
#'
#' @export
#'
#' @examples
#' # Load data example
#' # Occurrences of Puma concolor from the atlanticr R package
#' data("puma_atlanticr", package = "RuHere")
#' # Create metadata to standardize the occurrences
#' puma_metadata <- create_metadata(occ = puma_atlanticr,
#'                                  scientificName = "actual_species_name",
#'                                  decimalLongitude = "longitude",
#'                                  decimalLatitude = "latitude",
#'                                  elevation = "altitude",
#'                                  country = "country",
#'                                  stateProvince = "state",
#'                                  municipality = "municipality",
#'                                  locality = "study_location",
#'                                  year = "year_finish",
#'                                  habitat = "vegetation_type",
#'                                  datasetName = "reference")
#' # Now, we can use this metadata to standardize the columns
#' puma_occ <- format_columns(occ = puma_atlanticr, metadata = puma_metadata,
#'                            data_source = "atlanticr")
#'
create_metadata <- function(occ, scientificName,
                            decimalLongitude, decimalLatitude,
                            collectionCode = NA, catalogNumber = NA,
                            coordinateUncertaintyInMeters = NA, elevation = NA,
                            country = NA, stateProvince = NA, municipality = NA,
                            locality = NA, year = NA,
                            eventDate = NA, recordedBy = NA, identifiedBy = NA,
                            basisOfRecord = NA, occurrenceRemarks = NA,
                            habitat = NA, datasetName = NA, datasetKey = NA,
                            key = NA){

  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).")
  } else if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' should be a data.frame or data.table, not ", class(occ))
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # Check if columns are in occ
  # helper: check optional column
  check_optional_col <- function(arg, arg_name, occ) {
    if (!is.na(arg)) {
      if (!inherits(arg, "character"))
        stop("'", arg_name, "' must be a character, not ", class(arg))

      if (!arg %in% names(occ))
        stop("Column specified in '", arg_name, "' (", arg, ") was not found in 'occ'.")
    }
  }

  # ---- Required arguments ----
  if (!inherits(scientificName, "character"))
    stop("'scientificName' must be a character, not ", class(scientificName))
  if (!scientificName %in% names(occ))
    stop("Column specified in 'scientificName' (", scientificName, ") was not found in 'occ'.")

  if (!inherits(decimalLongitude, "character"))
    stop("'decimalLongitude' must be a character, not ", class(decimalLongitude))
  if (!decimalLongitude %in% names(occ))
    stop("Column specified in 'decimalLongitude' (", decimalLongitude, ") was not found in 'occ'.")

  if (!inherits(decimalLatitude, "character"))
    stop("'decimalLatitude' must be a character, not ", class(decimalLatitude))
  if (!decimalLatitude %in% names(occ))
    stop("Column specified in 'decimalLatitude' (", decimalLatitude, ") was not found in 'occ'.")

  # ---- Optional arguments checked via loop ----
  optional_args <- list(
    collectionCode = collectionCode,
    catalogNumber = catalogNumber,
    coordinateUncertaintyInMeters = coordinateUncertaintyInMeters,
    elevation = elevation,
    country = country,
    stateProvince = stateProvince,
    municipality = municipality,
    locality = locality,
    year = year,
    eventDate = eventDate,
    recordedBy = recordedBy,
    identifiedBy = identifiedBy,
    basisOfRecord = basisOfRecord,
    occurrenceRemarks = occurrenceRemarks,
    habitat = habitat,
    datasetName = datasetName,
    datasetKey = datasetKey,
    key = key
  )

  for (arg_name in names(optional_args)) {
    check_optional_col(optional_args[[arg_name]], arg_name, occ)
  }

  # Make data.frame
  d <- data.frame("scientificName" = scientificName,
                  "collectionCode" = collectionCode,
                  "catalogNumber" = catalogNumber,
                  "decimalLongitude" = decimalLongitude,
                  "decimalLatitude" = decimalLatitude,
                  "coordinateUncertaintyInMeters" = coordinateUncertaintyInMeters,
                  "elevation" = elevation,
                  "country" = country,
                  "stateProvince" = stateProvince,
                  "municipality" = municipality,
                  "locality" = locality,
                  "year" = year,
                  "eventDate" = eventDate,
                  "recordedBy" = recordedBy,
                  "identifiedBy" = identifiedBy,
                  "basisOfRecord" = basisOfRecord,
                  "occurrenceRemarks" = occurrenceRemarks,
                  "habitat" = habitat,
                  "datasetName" = datasetName,
                  "datasetKey" = datasetKey,
                  "key" = key)
  return(d)
}
