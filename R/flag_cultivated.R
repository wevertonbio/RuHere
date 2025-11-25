#' Flag occurrence records of cultived individuals
#'
#' @description
#' This function identifies records of cultivated individuals based on record
#' description.
#'
#' @param occ (data.frame) a data frame containing the occurrence records to be
#' examined, preferably standardized using `format_columns()`. Must contain the
#' columns specified in `columns`.
#' @param columns columns (character) vector of column names in `occ` where the
#' function will search for cultivated-related expressions. Default is
#' `c("occurrenceRemarks", "habitat", "locality")`.
#' @param cultivated_terms (character) optional vector of additional terms that
#' indicate a cultivated individual. Default is NULL, meaning it will use the
#' cultivated-related expressions available in `RuHere::cultivated$cultivated`.
#' @param not_cultivated_terms (character) optional vector of additional terms
#' that indicate a non-cultivated individual. Default is NULL, meaning it will
#' use the non cultivated-related expressions available in
#' `RuHere::cultivated$not_cultivated`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame augmented with
#' a new column named \code{cultivated_flag}. Records identified as cultivated
#' receive \code{FALSE}, while all other records receive \code{TRUE}.
#' @export
#'
#' @importFrom stringi stri_detect_regex
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Flag fossil records
#' occ_cultivated <- flag_cultivated(occ = occurrences)
flag_cultivated <- function(occ,
                            columns = c("occurrenceRemarks", "habitat",
                                        "locality"),
                            cultivated_terms = NULL,
                            not_cultivated_terms = NULL){

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  ## --- Check inputs ---------------------------------------------------------

  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).")
  } else if (!inherits(occ, "data.frame")) {
    stop("'occ' should be a data.frame, not ", class(occ))
  }

  if (!inherits(columns, "character")) {
    stop("'columns' must be a character vector, not ", class(columns))
  }

  if (!is.null(cultivated_terms) && !inherits(cultivated_terms, "character")) {
    stop("'cultivated_terms' must be a character vector or NULL, not ",
         class(cultivated_terms))
  }

  if (!is.null(not_cultivated_terms) &&
      !inherits(not_cultivated_terms, "character")) {
    stop("'not_cultivated_terms' must be a character vector or NULL, not ",
         class(not_cultivated_terms))
  }

  # Check that all specified columns exist in occ
  if (!all(columns %in% colnames(occ))) {
    missing_cols <- columns[!columns %in% colnames(occ)]
    stop("The following columns were not found in 'occ': ",
         paste(missing_cols, collapse = ", "))
  }

  # Get cultivated and not cultivated expressions
  cultivated <- getExportedValue("RuHere", "cultivated")$cultivated
  not_cultivated <- getExportedValue("RuHere", "cultivated")$not_cultivated

  # Add more terms?
  if(!is.null(cultivated_terms)){
    cultivated <- c(RuHere::cultivated$cultivated, cultivated_terms)
  }
  if(!is.null(not_cultivated_terms)){
    not_cultivated <- c(RuHere::cultivated$not_cultivated, not_cultivated_terms)
  }

  # Combine in single vector
  cultivated <- paste(cultivated, collapse = "|")

  # First, check cultivated...
  cultivated_index <- sapply(columns, function(i){
    which(stringi::stri_detect_regex(occ[[i]],
                               pattern = cultivated))
  }, USE.NAMES = FALSE)
  cultivated_index <- unlist(cultivated_index)

  # If there are cultivated, check not cultivated
  if(length(cultivated_index) > 0){
    not_cultivated <- paste(not_cultivated, collapse = "|")
    not_cultivated_index <- sapply(columns, function(i){
      which(stringi::stri_detect_regex(occ[cultivated_index, i],
                                       pattern = not_cultivated))
    }, USE.NAMES = FALSE)
    not_cultivated_index <- unlist(not_cultivated_index)
    if(length(not_cultivated_index) > 0){
      cultivated_index <- cultivated_index[-not_cultivated_index]
    }
  }

  # Create column to save flags
  occ$cultivated_flag <- TRUE
  # Flag if necessary
  if(length(cultivated_index) > 0){
    occ$cultivated_flag[cultivated_index] <- FALSE
  }

  return(occ)
}
