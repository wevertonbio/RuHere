#' Get consensus across multiple flags
#'
#' @description
#' This functions creates a new column representing the consensus across
#' multiple flag columns. The consensus can be computed in two ways:
#' - `"all_true"`: A record is considered valid (`TRUE`) only if **all**
#'   specified flag are valid (`TRUE`).
#' - `"any_true"`: A record is considered valid (`TRUE`) if **at least one**
#'   specified flag is valid (`TRUE`).
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records that
#' has been processed by two or more flagging functions.
#' @param flags (character) a string vector with the names of the flags to be
#' used in the consensus evaluation. See details for see the options.
#' @param consensus_rule (character) A string specifying how the consensus
#' should be computed. Options are `"all_true"` (record is considered valid only
#' when **all** flags are `TRUE` or `"any_true"`(record is considered valid when
#' **at least one** flag is `TRUE`. Default is `"all_true"`
#' @param flag_name (character) name of the column that will store the
#' consensus result. Default is `"consensus_flag"`.
#' @param remove_flag_columns (logical) whether to remove the original flag
#'   columns specified in `flags` from the final output. Default is `FALSE`.
#'
#' @details
#' The following flags are available: correct_country, correct_state, cultivated,
#' fossil, inaturalist, faunabr, florabr, wcvp, iucn, duplicated, thin, .val,
#' .equ, .zer, .cap, .cen, .sea, .urb, .otl, .gbf, .inst, and .aohi.
#'
#' @returns
#' The original \code{occ} with an additional logical column defined by
#' `flag_name`, indicating the consensus result based on the selected
#' `consensus_rule`.
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occ_flagged", package = "RuHere")
#'
#' # Get consensus using florabr, wcvp, and iucn flags
#'  # Valid (TRUE) only when all flags are TRUE
#' occ_consensus_all <- flag_consensus(occ = occ_flagged,
#'                                     flags = c("florabr", "wcvp", "iucn"),
#'                                     consensus_rule = "all_true")
#' # Valid (TRUE) when at least one flag is TRUE
#' occ_consensus_any <- flag_consensus(occ = occ_flagged,
#'                                     flags = c("florabr", "wcvp", "iucn"),
#'                                     consensus_rule = "any_true")
#'
flag_consensus <- function(occ, flags, consensus_rule = "all_true",
                           flag_name = "consensus_flag",
                           remove_flag_columns = FALSE){

  ## --- Input validation ------------------------------------------------------

  # occ must be data.frame or data.table
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table.", call. = FALSE)
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # flags must be character
  if (!is.character(flags) || length(flags) == 0) {
    stop("'flags' must be a non-empty character vector.", call. = FALSE)
  }

  # Define allowed flag names
  allowed_flags <- c(
    "correct_country", "correct_state", "cultivated",
    "fossil", "inaturalist", "faunabr", "florabr",
    "wcvp", "iucn", "duplicated", "thin_geo", "thin_env", "year",
    ".val", ".equ", ".zer", ".cap", ".cen",
    ".sea", ".urb", ".otl", ".gbf", ".inst", ".aohi"
  )

  # Which are invalid?
  invalid_flags <- flags[!flags %in% allowed_flags]
  if (length(invalid_flags) > 0) {
    stop(
      "The following values in 'flags' are not valid flag names: ",
      paste(invalid_flags, collapse = ", "),
      "\nAllowed flags are: ", paste(allowed_flags, collapse = ", "),
      call. = FALSE
    )
  }

  # Add '_flags' for some columns
  to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                "inaturalist", "duplicated", "thin_geo", "thin_env", "year")

  flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

  # check that all flag columns exist in occ
  missing_flags <- flags[!flags %in% names(occ)]
  if (length(missing_flags) > 0) {
    stop(
      "The following flags do not exist in 'occ': ",
      paste(missing_flags, collapse = ", "),
      call. = FALSE
    )
  }

  # consensus_rule must be valid
  valid_rules <- c("all_true", "any_true")
  if (!is.character(consensus_rule) || length(consensus_rule) != 1 ||
      !consensus_rule %in% valid_rules) {
    stop(
      "'consensus_rule' must be one of: ",
      paste(valid_rules, collapse = ", "),
      call. = FALSE
    )
  }

  # flag_name must be a single character
  if (!is.character(flag_name) || length(flag_name) != 1) {
    stop("'flag_name' must be a single character string.", call. = FALSE)
  }

  # avoid overwriting existing columns unless intentional
  if (flag_name %in% names(occ)) {
    stop(
      "The column name specified in 'flag_name' already exists in 'occ'. ",
      "Choose a different name.",
      call. = FALSE
    )
  }

  # remove_flag_columns must be logical
  if (!is.logical(remove_flag_columns) || length(remove_flag_columns) != 1) {
    stop("'remove_flag_columns' must be a single logical value.", call. = FALSE)
  }


  # Apply consensus
  if(consensus_rule == "all_true"){
    consensus <-  apply(occ[, flags], 1, function(x) all(x, na.rm = FALSE))
  } else if (consensus_rule == "any_true") {
    consensus <- apply(occ[, flags], 1, function(x) any(x, na.rm = FALSE))
  }

  # Add new columns
  occ[[flag_name]] <- consensus

  if(remove_flag_columns){
    occ[, flags] <- NULL
  }

  return(occ)

}
