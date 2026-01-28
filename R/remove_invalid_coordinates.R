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
#' @param save_invalid (logical) whether to save the invalid (removed) records.
#' If `TRUE`, an `output_dir` must be provided. Default is `FALSE`.
#' @param output_dir (character) path to an existing directory where records with
#' invalid coordinates will be saved. Only used when `save_invalid = TRUE`.
#' @param overwrite (logical)  whether to overwrite existing files in
#' `output_dir`. Only used when `save_invalid = TRUE`. Default is `FALSE`.
#' @param output_format (character) output format for saving removed records.
#' Options are `".csv"` or `".gz"`. Only used when `save_invalid = TRUE`.
#' Default is `".gz"`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is `TRUE`.
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
#' @importFrom data.table fwrite
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
                                       return_invalid = TRUE,
                                       save_invalid = FALSE,
                                       output_dir = NULL,
                                       overwrite = FALSE,
                                       output_format = ".gz",
                                       verbose = FALSE){
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

  # save_invalid must be logical
  if (!is.logical(save_invalid) || length(save_invalid) != 1) {
    stop("'save_invalid' must be TRUE or FALSE.", call. = FALSE)
  }

  # check saving options
  if (save_invalid) {

    # output_dir must exist
    if (is.null(output_dir) || !is.character(output_dir)) {
      stop("You must provide a valid 'output_dir' when 'save_invalid = TRUE'.", call. = FALSE)
    }
    if (!dir.exists(output_dir)) {
      stop("'output_dir' does not exist: ", output_dir, call. = FALSE)
    }

    # check output_format
    if (!output_format %in% c(".csv", ".gz")) {
      stop("'output_format' must be either '.csv' or '.gz'.", call. = FALSE)
    }

    # overwrite must be logical
    if (!is.logical(overwrite) || length(overwrite) != 1) {
      stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
    }
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
    # Save invalid?
    if(save_invalid){
      # Build path to sabe
      p <- file.path(output_dir, paste0("Invalid coordinates", output_format))
      #Check if files exists
      if(!overwrite){
        f_exists <- file.exists(p)
        if(f_exists){
          stop("'Invalid coordinates' already exists in '", output_dir, "'.\n",
               "Delete the file, change the folder or set 'overwrite = TRUE'")
        }
      }
      # Save
      data.table::fwrite(x = occ[invalid,], file = p)
    }

    # Return?
    if(return_invalid){
      return(list(valid = occ[!invalid,],
                  invalid = occ[invalid,]))
    } else {
      return(occ[!invalid,])
    }
  } else {
    if(verbose){
      message("All coordinates are valid! Returning original 'occ'.")}
    return(occ)
  }
}
