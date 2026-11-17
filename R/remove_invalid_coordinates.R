#' Identify and remove invalid coordinates
#'
#' @description
#' This function identifies and removes invalid geographic coordinates,
#' including non-numeric values, `NA`, `NaN`, `Inf`, or empty values, and
#' coordinates outside the valid range for Earth (latitude > 90 or < -90, and
#' longitude > 180 or < -180).
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records.
#' @param long (character) column name in `occ` with the longitude. Default is
#' `"decimalLongitude"`.
#' @param lat (character) column name in `occ` with the latitude. Default is
#' `"decimalLatitude"`.
#' @param return_invalid (logical) whether to return a list containing records
#' that passed and failed this test. Default is `TRUE`.
#' @param save_invalid (logical) whether to save the records that failed this
#' test (i.e., flagged as `FALSE`). If `TRUE`, an `output_dir` must be
#' provided. Default is `FALSE`.
#' @param output_dir (character) path to an existing directory where records
#' flagged as `FALSE` will be saved. Only used when `save_invalid = TRUE`.
#' @param overwrite (logical) whether to overwrite existing files in
#' `output_dir`. Only used when `save_invalid = TRUE`. Default is `FALSE`.
#' @param output_format (character) output format for saving flagged records.
#' Options are `".csv"` or `".gz"`. Only used when `save_invalid = TRUE`.
#' Default is `".gz"`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is `FALSE`.
#'
#' @returns
#' If `return_invalid = FALSE`, returns an object of the same class as `occ`
#' (`data.frame` or `data.table`) containing only records with valid
#' coordinates.
#' If `return_invalid = TRUE` (default), returns a list with two elements:
#' \itemize{
#'   \item `valid` – the dataset with valid coordinates.
#'   \item `invalid` – the dataset with invalid coordinates (augmented with
#'   `invalid_coordinates = FALSE`).
#' }
#'
#' @export
#'
#' @importFrom data.table fwrite is.data.table as.data.table
#'
#' @examples
#' # Create fake data example
#' occ <- data.frame(
#'   species = "Araucaria angustifolia",
#'   decimalLongitude = c(-49.2, -190, -50.1, -48.5, NA),
#'   decimalLatitude  = c(-25.4, -25.0, 240, -26.1, NA)
#' )
#'
#' # Split valid and invalid coordinates
#' res <- remove_invalid_coordinates(occ)
#' res$valid
#' res$invalid
#'
#' # Return only valid records
#' occ_valid <- remove_invalid_coordinates(occ, return_invalid = FALSE)
remove_invalid_coordinates <- function(occ,
                                       long = "decimalLongitude",
                                       lat = "decimalLatitude",
                                       return_invalid = TRUE,
                                       save_invalid = FALSE,
                                       output_dir = NULL,
                                       overwrite = FALSE,
                                       output_format = ".gz",
                                       verbose = FALSE) {
  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  is_single_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  # 1. Check occ
  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be specified (must not be NULL or missing).", call. = FALSE)
  }
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table, not ",
         paste(class(occ), collapse = "/"), ".", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # 2. Check long and lat arguments
  if (!is_single_str(long)) {
    stop("'long' must be a single character string with the longitude column name.",
         call. = FALSE)
  }
  if (!is_single_str(lat)) {
    stop("'lat' must be a single character string with the latitude column name.",
         call. = FALSE)
  }

  missing_cols <- setdiff(c(long, lat), names(occ))
  if (length(missing_cols) > 0L) {
    stop("The following required column(s) are missing in 'occ': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # 3. Check logical flags
  if (!is_single_bool(return_invalid)) {
    stop("'return_invalid' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(save_invalid)) {
    stop("'save_invalid' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(verbose)) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # 4. Check saving options
  if (save_invalid) {
    if (!is_single_str(output_dir)) {
      stop("You must provide a valid 'output_dir' when 'save_invalid = TRUE'.", call. = FALSE)
    }
    if (!dir.exists(output_dir)) {
      stop("'output_dir' does not exist: ", output_dir, call. = FALSE)
    }
    if (!is_single_str(output_format) || !output_format %in% c(".csv", ".gz")) {
      stop("'output_format' must be either '.csv' or '.gz'.", call. = FALSE)
    }
    if (!is_single_bool(overwrite)) {
      stop("'overwrite' must be a single logical value (TRUE or FALSE).", call. = FALSE)
    }
  }

  was_dt <- data.table::is.data.table(occ)
  df <- if (was_dt) as.data.frame(occ) else occ

  # Coerce coordinates to numeric safely (without warning spam)
  if (!is.numeric(df[[long]])) {
    df[[long]] <- suppressWarnings(as.numeric(as.character(df[[long]])))
  }
  if (!is.numeric(df[[lat]])) {
    df[[lat]] <- suppressWarnings(as.numeric(as.character(df[[lat]])))
  }

  lon_v <- df[[long]]
  lat_v <- df[[lat]]

  # Identify invalid coordinates (NA, NaN, Inf, or out-of-bounds)
  invalid <- !is.finite(lon_v) | !is.finite(lat_v) |
    lon_v > 180 | lon_v < -180 |
    lat_v > 90 | lat_v < -90

  n_invalid <- sum(invalid)

  if (n_invalid == nrow(df)) {
    stop("All coordinates are invalid!", call. = FALSE)
  }

  if (n_invalid > 0L) {
    occ_invalid <- df[invalid, , drop = FALSE]
    occ_invalid$invalid_coordinates <- FALSE

    if (save_invalid) {
      p <- file.path(output_dir, paste0("Invalid coordinates", output_format))
      if (!overwrite && file.exists(p)) {
        stop("'Invalid coordinates' already exists in '", output_dir, "'.\n",
             "Delete the file, change the folder or set 'overwrite = TRUE'.",
             call. = FALSE)
      }
      data.table::fwrite(x = occ_invalid, file = p)
    }

    if (verbose) {
      message(n_invalid, " record(s) with invalid coordinates removed.")
    }

    occ_valid <- df[!invalid, , drop = FALSE]
    if (was_dt) {
      occ_valid <- data.table::as.data.table(occ_valid)
      occ_invalid <- data.table::as.data.table(occ_invalid)
    }

    if (return_invalid) {
      return(list(valid = occ_valid, invalid = occ_invalid))
    } else {
      return(occ_valid)
    }
  } else {
    if (verbose) {
      message("All coordinates are valid!")
    }
    occ_valid <- if (was_dt) data.table::as.data.table(df) else df
    if (return_invalid) {
      occ_empty <- df[0L, , drop = FALSE]
      occ_empty$invalid_coordinates <- logical(0L)
      if (was_dt) {
        occ_empty <- data.table::as.data.table(occ_empty)
      }
      return(list(valid = occ_valid, invalid = occ_empty))
    } else {
      return(occ_valid)
    }
  }
}
