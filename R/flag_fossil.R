#' Flag fossil records
#'
#' @description
#' This function identifies occurrence records that correspond to fossils,
#' based on specific search terms found in selected columns.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be examined, preferably standardized using `format_columns()`.
#' Must contain the columns specified in `columns`.
#' @param columns (character) vector of column names in `occ` where the function
#' will search for the term `"fossil"` or other fossil-related expressions.
#' Default is `c("basisOfRecord", "occurrenceRemarks")`.
#' @param fossil_terms (character) optional vector of additional terms that
#' indicate a fossil record (e.g., `"paleontological"`, `"subfossil"`). Default
#' is `NULL`.
#'
#' @returns
#' A `data.frame` that is the original `occ` data frame augmented with a new
#' column named `fossil_flag`. Records identified as fossils receive `FALSE`,
#' while all other records receive `TRUE`. As with all other flagging functions
#' in RuHere, `TRUE` indicates that the record passed this test and is eligible
#' for retention; `FALSE` indicates it failed and is flagged as potentially
#' problematic, and can be removed using `remove_flagged()`.
#'
#' @export
#'
#' @importFrom stringi stri_detect_regex stri_opts_regex
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Flag fossil records
#' occ_fossil <- flag_fossil(occ = occurrences)
flag_fossil <- function(occ,
                        columns = c("basisOfRecord", "occurrenceRemarks"),
                        fossil_terms = NULL) {

  # ---- ARGUMENT CHECKING ----
  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be specified (must not be NULL or missing).", call. = FALSE)
  }
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' should be a data.frame or data.table, not ",
         paste(class(occ), collapse = "/"), ".", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }
  if (!is.character(columns) || length(columns) == 0L || any(is.na(columns))) {
    stop("'columns' must be a non-empty character vector without NAs.", call. = FALSE)
  }

  missing_cols <- setdiff(columns, names(occ))
  if (length(missing_cols) > 0L) {
    stop("The following columns specified in 'columns' are not present in 'occ': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.null(fossil_terms) && (!is.character(fossil_terms) || any(is.na(fossil_terms)))) {
    stop("'fossil_terms' must be a character vector without NAs or NULL.", call. = FALSE)
  }

  # Force occ to be a data.frame
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  # Build case-insensitive regex pattern
  terms <- unique(c("fossil", fossil_terms))
  terms <- terms[nzchar(trimws(terms))]
  pattern <- paste(terms, collapse = "|")
  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)

  # Vectorized unique-value lookup across selected columns
  is_fossil <- rep(FALSE, nrow(occ))
  for (col in columns) {
    vals <- as.character(occ[[col]])
    u_vals <- unique(vals[!is.na(vals)])
    if (length(u_vals) > 0L) {
      matched_u <- u_vals[stringi::stri_detect_regex(u_vals, pattern = pattern, opts_regex = opts)]
      if (length(matched_u) > 0L) {
        is_fossil <- is_fossil | (vals %in% matched_u)
      }
    }
  }

  occ$fossil_flag <- !is_fossil
  return(occ)
}
