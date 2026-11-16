#' Flag records outside a year range
#'
#' @description
#' This function identifies occurrence records collected before or after
#' user-specified years.
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records,
#' preferably standardized using `format_columns()`. Must contain the column
#' specified in `year_column`.
#' @param year_column (character) name of the column containing the year in
#' which the occurrence was recorded. This column must be numeric. Default is
#' `"year"`.
#' @param lower_limit (numeric) the minimum acceptable year. Records collected
#' before this value will be flagged. Default is `NULL`.
#' @param upper_limit (numeric) the maximum acceptable year. Records collected
#' after this value will be flagged. Default is `NULL`.
#' @param flag_NA (logical) whether to flag records with missing year
#' information. Default is `FALSE`.
#'
#' @returns
#' A `data.frame` identical to `occ` but with an additional column named
#' `year_flag`. Records collected outside the year range specified are
#' assigned `FALSE`. As with all other flagging functions in RuHere, `TRUE`
#' indicates that the record passed this test (i.e., falls within the
#' specified year range) and is eligible for retention; `FALSE` indicates it
#' failed and is flagged as potentially problematic, and can be removed using
#' `remove_flagged()`.
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Flag records collected before 1980 and after 2010
#' occ_year <- flag_year(occ = occurrences, lower_limit = 1980,
#'                       upper_limit = 2010)
flag_year <- function(occ,
                      year_column = "year",
                      lower_limit = NULL,
                      upper_limit = NULL,
                      flag_NA = FALSE) {

  # ---- ARGUMENT CHECKING ----
  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).", call. = FALSE)
  }
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table, not ",
         paste(class(occ), collapse = "/"), ".", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }
  if (!is.character(year_column) || length(year_column) != 1L || is.na(year_column)) {
    stop("'year_column' must be a single character string.", call. = FALSE)
  }
  if (!year_column %in% names(occ)) {
    stop("Column '", year_column, "' not found in 'occ'.", call. = FALSE)
  }
  if (!is.numeric(occ[[year_column]])) {
    stop("The column specified in 'year_column' must be numeric.", call. = FALSE)
  }
  if (!is.null(lower_limit) && (!is.numeric(lower_limit) || length(lower_limit) != 1L || is.na(lower_limit))) {
    stop("'lower_limit' must be a single numeric value or NULL.", call. = FALSE)
  }
  if (!is.null(upper_limit) && (!is.numeric(upper_limit) || length(upper_limit) != 1L || is.na(upper_limit))) {
    stop("'upper_limit' must be a single numeric value or NULL.", call. = FALSE)
  }
  if (!is.null(lower_limit) && !is.null(upper_limit) && lower_limit > upper_limit) {
    stop("'lower_limit' cannot be greater than 'upper_limit'.", call. = FALSE)
  }
  if (!is.logical(flag_NA) || length(flag_NA) != 1L || is.na(flag_NA)) {
    stop("'flag_NA' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # Force occ to be a data.frame
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  yrs <- occ[[year_column]]
  valid_year <- rep(TRUE, nrow(occ))

  if (!is.null(lower_limit)) {
    valid_year[!is.na(yrs) & yrs < lower_limit] <- FALSE
  }
  if (!is.null(upper_limit)) {
    valid_year[!is.na(yrs) & yrs > upper_limit] <- FALSE
  }
  if (flag_NA) {
    valid_year[is.na(yrs)] <- FALSE
  }

  occ$year_flag <- valid_year
  return(occ)
}
