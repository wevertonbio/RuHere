#' Flag occurrence records of cultivated individuals
#'
#' @description
#' This function identifies records of cultivated individuals based on record
#' description.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be examined, preferably standardized using `format_columns()`.
#' Must contain the columns specified in `columns`.
#' @param columns (character) vector of column names in `occ` where the
#' function will search for cultivated-related expressions. Default is
#' `c("occurrenceRemarks", "habitat", "locality")`.
#' @param cultivated_terms (character) optional vector of additional terms that
#' indicate a cultivated individual. Default is `NULL`, meaning it will use the
#' cultivated-related expressions available in `RuHere::cultivated$cultivated`.
#' @param not_cultivated_terms (character) optional vector of additional terms
#' that indicate a non-cultivated individual. Default is `NULL`, meaning it will
#' use the non-cultivated-related expressions available in
#' `RuHere::cultivated$not_cultivated`.
#'
#' @returns
#' A `data.frame` that is the original `occ` data frame augmented with a new
#' column named `cultivated_flag`. Records identified as cultivated receive
#' `FALSE`, while all other records receive `TRUE`. As with all other flagging
#' functions in RuHere, `TRUE` indicates that the record passed this test and
#' is eligible for retention; `FALSE` indicates it failed and is flagged as
#' potentially problematic, and can be removed using `remove_flagged()`.
#'
#' @export
#'
#' @importFrom stringi stri_detect_regex stri_opts_regex
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Flag cultivated records
#' occ_cultivated <- flag_cultivated(occ = occurrences)
flag_cultivated <- function(occ,
                            columns = c("occurrenceRemarks", "habitat",
                                        "locality"),
                            cultivated_terms = NULL,
                            not_cultivated_terms = NULL) {

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
  if (!is.null(cultivated_terms) && (!is.character(cultivated_terms) || any(is.na(cultivated_terms)))) {
    stop("'cultivated_terms' must be a character vector without NAs or NULL.", call. = FALSE)
  }
  if (!is.null(not_cultivated_terms) && (!is.character(not_cultivated_terms) || any(is.na(not_cultivated_terms)))) {
    stop("'not_cultivated_terms' must be a character vector without NAs or NULL.", call. = FALSE)
  }

  missing_cols <- setdiff(columns, names(occ))
  if (length(missing_cols) > 0L) {
    stop("The following columns were not found in 'occ': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Force occ to be a data.frame
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  # Retrieve reference terms
  cult_dict <- getExportedValue("RuHere", "cultivated")
  cultivated <- unique(c(cult_dict$cultivated, cultivated_terms))
  not_cultivated <- unique(c(cult_dict$not_cultivated, not_cultivated_terms))

  cultivated <- cultivated[!is.na(cultivated) & nzchar(trimws(cultivated))]
  not_cultivated <- not_cultivated[!is.na(not_cultivated) & nzchar(trimws(not_cultivated))]

  cult_pattern <- paste(cultivated, collapse = "|")
  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)

  # Step 1: Detect cultivated records via unique values per column
  is_cultivated <- rep(FALSE, nrow(occ))
  for (col in columns) {
    vals <- as.character(occ[[col]])
    u_vals <- unique(vals[!is.na(vals)])
    if (length(u_vals) > 0L) {
      matched_u <- u_vals[stringi::stri_detect_regex(u_vals, pattern = cult_pattern, opts_regex = opts)]
      if (length(matched_u) > 0L) {
        is_cultivated <- is_cultivated | (vals %in% matched_u)
      }
    }
  }

  # Step 2: Exclude false positives matching not_cultivated terms (subsetting only candidates)
  if (any(is_cultivated) && length(not_cultivated) > 0L) {
    not_cult_pattern <- paste(not_cultivated, collapse = "|")
    cand_idx <- which(is_cultivated)
    is_not_cult <- rep(FALSE, length(cand_idx))

    for (col in columns) {
      vals <- as.character(occ[[col]][cand_idx])
      u_vals <- unique(vals[!is.na(vals)])
      if (length(u_vals) > 0L) {
        matched_u <- u_vals[stringi::stri_detect_regex(u_vals, pattern = not_cult_pattern, opts_regex = opts)]
        if (length(matched_u) > 0L) {
          is_not_cult <- is_not_cult | (vals %in% matched_u)
        }
      }
    }
    is_cultivated[cand_idx[is_not_cult]] <- FALSE
  }

  occ$cultivated_flag <- !is_cultivated
  return(occ)
}
