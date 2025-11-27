#' Flag fossil records
#'
#' @description
#' This function identifies occurrence records that correspond to fossils,
#' based on specific search terms found in selected columns.
#'
#' @param occ (data.frame) a data frame containing the occurrence records to be
#' examined, preferably standardized using `format_columns()`. Must contain the
#' columns specified in `columns`.
#' @param columns (character) vector of column names in `occ` where the function
#' will search for the term `"fossil"` or other fossil-related expressions.
#' Default is `c("basisOfRecord", "occurrenceRemarks")`.
#' @param fossil_terms (character) optional vector of additional terms that
#' indicate a fossil record (e.g., `"paleontological"`, `"subfossil"`). Default
#' is `NULL`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame augmented with
#' a new column named \code{fossil_flag}. Records identified as fossils receive
#' \code{FALSE}, while all other records receive \code{TRUE}.
#'
#' @export
#'
#' @importFrom stringi stri_detect_regex
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Flag fossil records
#' occ_fossil <- flag_fossil(occ = occurrences)
flag_fossil <- function(occ,
                        columns = c("basisOfRecord", "occurrenceRemarks"),
                        fossil_terms = NULL){


  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  ### --- Argument checking --- ###

  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be specified (must not be NULL or missing).")
  } else if (!inherits(occ, "data.frame")) {
    stop("'occ' should be a data.frame, not ", class(occ))
  }

  if (!inherits(columns, "character")) {
    stop("'columns' should be a character, not ", class(columns))
  }

  # check that columns exist in occ
  missing_cols <- columns[!columns %in% names(occ)]
  if (length(missing_cols) > 0) {
    stop(
      "The following columns specified in 'columns' are not present in 'occ': ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!is.null(fossil_terms) && !inherits(fossil_terms, "character")) {
    stop("'fossil_terms' should be a character or NULL, not ", class(fossil_terms))
  }


  # Combine fossil terms in single vector
  if(is.null(fossil_terms)){
    fossil <- "fossil"
  } else {
    fossil <- c("fossil", fossil_terms)
  }

  fossil <- paste(fossil, collapse = "|")

  # First, check fossil...
  fossil_index <- sapply(columns, function(i){
    which(stringi::stri_detect_regex(occ[[i]],
                                     pattern = fossil))
  }, USE.NAMES = FALSE)
  fossil_index <- unlist(fossil_index)

  # Create column to save flags
  occ$fossil_flag <- TRUE
  # Flag if necessary
  if(length(fossil_index) > 0){
    occ$fossil_flag[fossil_index] <- FALSE
  }

  return(occ)
}
