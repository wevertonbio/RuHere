#' @title get_idigbio
#'
#' @usage get_idigbio(species = NULL, fields = "all", genus = NULL,
#' family = NULL, order = NULL, phylum = NULL, kingdom = NULL, country = NULL,
#' county = NULL, limit = NULL, offset = NULL, dir, filename = "idigbio_output",
#' save = FALSE, compress = FALSE, file.format = "csv", ...)
#'
#' @description
#' Downloads species occurrence records from the iDigBio (Integrated Digitized
#' Biocollections) database with flexible taxonomic and geographic filtering
#' options.
#'
#' @param species (character) scientific name(s) of species to search for.
#' Default is `NULL`.
#' @param fields (character) fields to retrieve from iDigBio. Default is `"all"`.
#' @param genus (character) genus name for filtering results. Default is `NULL`.
#' @param family (character) family name for filtering results. Default is `NULL`.
#' @param order (character) order name for filtering results. Default is `NULL`.
#' @param phylum (character) phylum name for filtering results. Default is `NULL`.
#' @param kingdom (character) kingdom name for filtering results. Default is `NULL`.
#' @param country (character) country name for geographic filtering. Default is `NULL`.
#' @param county (character) county name for geographic filtering. Default is `NULL`.
#' @param limit (numeric) maximum number of records to retrieve. Default is
#' `NULL` (no limit).
#' @param offset (numeric) number of records to skip before starting retrieval.
#' Default is `NULL` (starts at 0).
#' @param dir (character) directory path where the file will be saved.
#' Required if `save = TRUE`.
#' @param filename (character) name of the output file without extension.
#' Default is `"idigbio_output"`.
#' @param save (logical) if `TRUE`, saves the results to a CSV file.
#' Default is `FALSE`.
#' @param compress (logical) if `TRUE` and `save = TRUE`, compresses the output
#' file as .csv.zip. Default is `FALSE`.
#' @param file.format (character) file format for saving output (`"csv"`, `"rds"`).
#' Default is `"csv"`
#' @param ... additional arguments passed to `ridigbio::idig_search_records()`.
#'
#' @return
#' A \code{data.frame} containing occurrence records from iDigBio with the requested
#' fields.
#'
#' @importFrom ridigbio idig_search_records
#' @importFrom data.table fwrite
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ## search for a single species
#' records_basic <- get_idigbio(species = "Arecaceae")
#'
#' ## search for multiple species
#' records_multiple <- get_idigbio(
#'   species = c("Araucaria angustifolia"),
#'   limit = 100)
#'
#' ## save results as a compressed RDS file
#' records_saved_rds <- get_idigbio(
#'   species = "Anacardiaceae",
#'   limit = 50,
#'   dir = tempdir(),
#'   filename = "anacardiaceae_records",
#'   save = TRUE,
#'   compress = TRUE,
#'   file.format = "rds")
#'}
#'
get_idigbio <- function(species = NULL, fields = "all",
                        genus = NULL, family = NULL,
                        order = NULL, phylum = NULL, kingdom = NULL,
                        country = NULL, county = NULL, limit = NULL,
                        offset = NULL, dir, filename = "idigbio_output",
                        save = FALSE, compress = FALSE, file.format = "csv",
                        ...) {

  if (!is.null(species) && !inherits(species, "character")) {
    stop("'species' must be a character or NULL, not ", class(species))
  }

  if (!is.null(fields) && !inherits(fields, "character")) {
    stop("'fields' must be a character or NULL, not ", class(fields))
  }

  if (!is.null(genus) && !inherits(genus, "character")) {
    stop("'genus' must be a character or NULL, not ", class(genus))
  }

  if (!is.null(family) && !inherits(family, "character")) {
    stop("'family' must be a character or NULL, not ", class(family))
  }

  if (!is.null(order) && !inherits(order, "character")) {
    stop("'order' must be a character or NULL, not ", class(order))
  }

  if (!is.null(phylum) && !inherits(phylum, "character")) {
    stop("'phylum' must be a character or NULL, not ", class(phylum))
  }

  if (!is.null(kingdom) && !inherits(kingdom, "character")) {
    stop("'kingdom' must be a character or NULL, not ", class(kingdom))
  }

  if (!is.null(country) && !inherits(country, "character")) {
    stop("'country' must be a character or NULL, not ", class(country))
  }

  if (!is.null(county) && !inherits(county, "character")) {
    stop("'county' must be a character or NULL, not ", class(county))
  }

  if (!is.null(limit) && !inherits(limit, "numeric")) {
    stop("'limit' must be numeric or NULL, not ", class(limit))
  }

  if (!is.null(offset) && !inherits(offset, "numeric")) {
    stop("'offset' must be numeric or NULL, not ", class(offset))
  }

  if (!inherits(filename, "character")) {
    stop("'filename' must be a character, not ", class(filename))
  }

  if (!inherits(save, "logical")) {
    stop("'save' must be logical, not ", class(save))
  }

  if (!inherits(compress, "logical")) {
    stop("'compress' must be logical, not ", class(compress))
  }

  if (isTRUE(save)) {
    if (missing(dir) || is.null(dir)) {
      stop("'dir' is required (must not be NULL or missing) when save = TRUE.")
    }
    if (!inherits(dir, "character") || length(dir) != 1) {
      stop("'dir' must be a single character string when save = TRUE, not ", class(dir))
    }
    if (!dir.exists(dir)) {
      stop(paste0("Directory '", dir, "' does not exist. It must be created before saving."))
    }
  }

  if (!file.format %in% c("csv", "rds"))
    stop("'file.format' must be either 'csv' or 'rds'")

  rq = list()

  if (!is.null(species)) rq$scientificname <- species

  if (!is.null(genus)) rq$genus <- genus

  if (!is.null(family)) rq$family <- family

  if (!is.null(order)) rq$order <- order

  if (!is.null(phylum)) rq$phylum <- phylum

  if (!is.null(kingdom)) rq$kingdom <- kingdom

  if (!is.null(country)) rq$country <- country

  if (!is.null(county)) rq$county <- county

  if (is.null(limit)) limit <- 0

  if (is.null(offset)) offset <- 0

  df <- ridigbio::idig_search_records(rq=rq, fields = fields,
                                      limit = limit, offset = offset, ...)

  firstup <- function (x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
  }

  df$scientificname <- firstup(df$scientificname)

  colnames(df)[colnames(df) == "geopoint.lon"] <- "lon"
  colnames(df)[colnames(df) == "geopoint.lat"] <- "lat"
  colnames(df)[colnames(df) == "data.dwc:eventDate"] <- "eventDate"
  colnames(df)[colnames(df) == "data.dwc:year"] <- "year"
  colnames(df)[colnames(df) == "data.dwc:month"] <- "month"
  colnames(df)[colnames(df) == "data.dwc:day"] <- "day"

  if (save) {

    if (file.format == "csv") {
      if (compress) {
        fullname <- file.path(dir, paste0(filename, ".csv.zip"))
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname, compress = "gzip")
      }
      else {
        fullname <- file.path(dir, paste0(filename, ".csv"))
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname)
      }
    }

    if (file.format == "rds") {
      fullname <- file.path(dir, paste0(filename, ".rds"))
      message(paste0("Writing ", fullname, " on disk."))
      if (compress) {
        saveRDS(df, file = fullname, compress = "gzip")
      }
      else {
        saveRDS(df, file = fullname, compress = FALSE)
      }
    }

  }

  return(df)
}
