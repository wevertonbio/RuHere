#' Flag occurrence records sourced from iNaturalist
#'
#' @description
#' This function identifies and flags occurrence records sourced from
#' iNaturalist. It can flag all iNaturalist records or only those that do not
#' have Research Grade status.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be examined, preferably standardized using `format_columns()`.
#' Must contain the columns specified in `columns`.
#' @param columns (character) column name(s) in `occ` where the function will
#' search for the term `"iNaturalist"`. Default is `"datasetName"`.
#' @param research_grade (logical) whether to flag *all* records from
#' iNaturalist, including those with Research Grade status. Default is `FALSE`,
#' meaning that only iNaturalist records **without** Research Grade will be
#' flagged.
#'
#' @details
#' According to [iNaturalist](https://help.inaturalist.org/en/support/solutions/articles/151000169936-what-is-the-data-quality-assessment-and-how-do-observations-qualify-to-become-research-grade-),
#' Observations become Research Grade when:
#' - the iNaturalist community agrees on species-level ID or lower, i.e. when
#' more than 2/3 of identifiers agree on a taxon;
#' - the community taxon and the observation taxon agree;
#' - or the community agrees on an ID between family and species and votes that
#' the community taxon is as good as it can be.
#'
#' @returns
#' A `data.frame` that is the original `occ` data frame augmented with a new
#' column named `inaturalist_flag`. Flagged records receive `FALSE`, while all
#' other records receive `TRUE`. As with all other flagging functions in
#' RuHere, `TRUE` indicates that the record passed this test and is eligible
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
#' # Flag only iNaturalist records without Research Grade
#' occ_inat <- flag_inaturalist(occ = occurrences, research_grade = FALSE)
#' table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)
#' # Flag all iNaturalist records (including Research Grade)
#' occ_inat <- flag_inaturalist(occ = occurrences, research_grade = TRUE)
#' table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)
flag_inaturalist <- function(occ,
                             columns = "datasetName",
                             research_grade = FALSE) {

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
    stop("The following columns were not found in 'occ': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.logical(research_grade) || length(research_grade) != 1L || is.na(research_grade)) {
    stop("'research_grade' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # Force occ to be a data.frame
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)

  # Step 1: Identify iNaturalist records via unique values
  is_inat <- rep(FALSE, nrow(occ))
  for (col in columns) {
    vals <- as.character(occ[[col]])
    u_vals <- unique(vals[!is.na(vals)])
    if (length(u_vals) > 0L) {
      matched_u <- u_vals[stringi::stri_detect_regex(u_vals, pattern = "inaturalist", opts_regex = opts)]
      if (length(matched_u) > 0L) {
        is_inat <- is_inat | (vals %in% matched_u)
      }
    }
  }

  # Step 2: Unflag Research Grade records when research_grade = FALSE
  if (any(is_inat) && !research_grade) {
    rg_pattern <- "research[-_ ]?grade"
    cand_idx <- which(is_inat)
    is_rg <- rep(FALSE, length(cand_idx))

    for (col in columns) {
      vals <- as.character(occ[[col]][cand_idx])
      u_vals <- unique(vals[!is.na(vals)])
      if (length(u_vals) > 0L) {
        matched_u <- u_vals[stringi::stri_detect_regex(u_vals, pattern = rg_pattern, opts_regex = opts)]
        if (length(matched_u) > 0L) {
          is_rg <- is_rg | (vals %in% matched_u)
        }
      }
    }
    is_inat[cand_idx[is_rg]] <- FALSE
  }

  occ$inaturalist_flag <- !is_inat
  return(occ)
}
