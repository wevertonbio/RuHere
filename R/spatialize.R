#' Spatialize occurrence records
#'
#' @description
#' Convert a data.frame (or data.table) of occurrence records into a
#' **SpatVector** object.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be flagged. Must contain columns for longitude, and latitude.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param crs (character) the coordinate reference system in one of the
#' following formats: PROJ-string notation, WKT/WKT2, or <authority>:<code>
#' (see `?terra::crs`). Default is "epsg:4326".
#' @param force_numeric (logical) whether to coerce the longitude and latitude
#' columns to numeric if they are not already. Default is `TRUE.`
#'
#' @returns
#' A **SpatVector** object containing the spatialized occurrence records.
#'
#' @importFrom terra vect
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Spatialize the occurrence records
#' pts <- spatialize(occurrences)
#' # Plot the resulting SpatVector
#' terra::plot(pts)
#'
spatialize <- function(occ, long = "decimalLongitude", lat = "decimalLatitude",
                       crs = "epsg:4326", force_numeric = TRUE){

  # --- Argument checking ------------------------------------------------------

  # occ must be data.frame or data.table
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table with occurrence records.")
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # long and lat must exist in occ
  if (!all(c(long, lat) %in% names(occ))) {
    stop("Columns '", long, "' and/or '", lat, "' not found in 'occ'.")
  }

  # CRS must be valid
  if (!is.character(crs) || length(crs) != 1) {
    stop("'crs' must be a single character string indicating a coordinate reference system.")
  }

  # force_numeric
  if (!inherits(force_numeric, "logical")) {
    stop("'force_numeric' must be logical, not ", class(force_numeric))
  }

  if(force_numeric){
    if(!inherits(occ[[long]], "numeric"))
      occ[[long]] <- as.numeric(occ[[long]])
    if(!inherits(occ[[lat]], "numeric"))
      occ[[lat]] <- as.numeric(occ[[lat]])
  }

  # Remove NAs
  occ <- occ[!is.na(occ[[long]]) & !is.na(occ[[lat]]), ]
  if(nrow(occ) == 0){
    stop("'occ' does not have any valid coordinates")
  }

  terra::vect(occ, geom = c(x = long, y = lat), crs = crs)
}
