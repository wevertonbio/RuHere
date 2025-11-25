#' Flag records outside a year range
#'
#' @description
#' This function identifies occurrence records collected before or after
#' user-specified years.
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably
#' standardized using `format_columns()`. Must contain the column specified in
#' `year_column`.
#' @param year_column (character) name of the column containing the year in
#' which the occurrence was recorded. This column must be numeric.
#' @param lower_limit (numeric) the minimum acceptable year. Records collected
#' before this value will be flagged. Default is `NULL`.
#' @param upper_limit (numeric) the maximum acceptable year. Records collected
#' after this value will be flagged. Default is `NULL`.
#' @param flag_NA (character) whether to flag records with missing year
#' information. Default is `FALSE`.
#'
#' @returns
#' A data.frame identical to `occ` but with an additional column named
#' `year_flag`. Records collected outside the year range specified are assigned
#' `FALSE`.
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
                      flag_NA = FALSE){

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # Checking block
  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).")
  } else if (!inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame, not ", class(occ))
  }

  if (!inherits(year_column, "character")) {
    stop("'year_column' must be a character, not ", class(year_column))
  }

  if (!year_column %in% names(occ)) {
    stop("Column '", year_column, "' not found in 'occ'.")
  }

  if (!is.numeric(occ[[year_column]])) {
    stop("The column specified in 'year_column' must be numeric.")
  }

  if (!is.null(lower_limit) && !inherits(lower_limit, "numeric")) {
    stop("'lower_limit' must be numeric or NULL, not ", class(lower_limit))
  }

  if (!is.null(upper_limit) && !inherits(upper_limit, "numeric")) {
    stop("'upper_limit' must be numeric or NULL, not ", class(upper_limit))
  }

  if (!inherits(flag_NA, "logical")) {
    stop("'flag_NA' must be logical, not ", class(flag_NA))
  }


  # Create column to flag
  occ$year_flag <- TRUE
  if(!is.null(lower_limit)){
    occ$year_flag[occ[[year_column]] < lower_limit &
                    !is.na(occ[[year_column]])] <- FALSE
  }

  if(!is.null(upper_limit)){
    occ$year_flag[occ[[year_column]] > upper_limit &
                    !is.na(occ[[year_column]])] <- FALSE
  }

  if(flag_NA){
    occ$year_flag[is.na(occ[[year_column]])] <- FALSE
  }

  return(occ)
}
