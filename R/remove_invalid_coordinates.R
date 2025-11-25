#' Identify and remove invalid coordinates
#'
#' @description
#' This function identifies and removes invalid geographic coordinates,
#' including non-numeric values, NA or empty values, and coordinates outside the
#' valid range for Earth (latitude > 90 or < -90, and longitude > 180 or < -180).
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records.
#' @param long (character) column name in `occ` with the longitude.
#' @param lat (character) column name in `occ` with the latitude.
#' @param return_invalid (logical) whether to return a list containing the valid
#' and invalid coordinates. Default is TRUE.
#'
#' @returns
#' If `return_invalid = FALSE`, returns the occurrence dataset containing only
#' valid coordinates.
#' If `return_invalid = TRUE` (default), returns a list with two elements:
#' \itemize{
#'   \item `valid` – the dataset with valid coordinates.
#'   \item `invalid` – the dataset with invalid coordinates removed.
#' }
#'
#' @export
#'
#' @examples
#' # Create fake data example
#' occ <- data.frame("species" = "spp",
#'                   "decimalLongitude" = c(10, -190, 20, 50, NA),
#'                   "decimalLatitude" = c(20, 20, 240, 50, NA))
#' # Split valid and invalid coordinates
#' occ_valid <- remove_invalid_coordinates(occ)
remove_invalid_coordinates <- function(occ,
                                       long = "decimalLongitude",
                                       lat = "decimalLatitude",
                                       return_invalid = TRUE){
  ## ---- Argument checking -----------------------------------------------------

  # check occ
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table.", call. = FALSE)
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # check long and lat arguments
  if (!is.character(long) || length(long) != 1) {
    stop("'long' must be a single character string with the longitude column name.",
         call. = FALSE)
  }

  if (!is.character(lat) || length(lat) != 1) {
    stop("'lat' must be a single character string with the latitude column name.",
         call. = FALSE)
  }

  # check if longitude and latitude columns exist
  missing_cols <- setdiff(c(long, lat), names(occ))
  if (length(missing_cols) > 0) {
    stop(
      "The following required column(s) are missing in 'occ': ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # check return_invalid
  if (!is.logical(return_invalid) || length(return_invalid) != 1) {
    stop("'return_invalid' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }


  # Force numeric
  if(!inherits(occ[[long]], "numeric"))
    occ[[long]] <- as.numeric(occ[[long]])
  if(!inherits(occ[[lat]], "numeric"))
    occ[[lat]] <- as.numeric(occ[[lat]])

  # Identify invalids
  invalid <- occ[[long]] > 180 | occ[[long]] < -180 |
                     occ[[lat]] > 90 | occ[[lat]] < -90 |
                     is.na(occ[[long]]) | is.na(occ[[lat]])

  if(sum(invalid) == nrow(occ)){
    stop("All coordinates are invalid!")
  }

  if(sum(invalid) > 0){
    if(return_invalid){
      return(list(valid = occ[!invalid,],
                  invalid = occ[invalid,]))
    } else {
      return(occ[!invalid,])
    }
  } else {
    message("All coordinates are valid! Returning original 'occ'.")
    return(occ)
  }
}
