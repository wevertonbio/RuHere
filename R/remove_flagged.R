#' Remove flagged records
#'
#' @description
#' This function removes occurrence records flagged as invalid by one or more
#' flagging functions. Additional manual control is available to force keeping
#' or removing specific records, regardless of their flag values.
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records that
#' has been processed by one or more flagging functions. See *Details*.
#' @param flags (character) a character vector with the names of the flag
#' columns to be used for filtering records. See *Details* for the available
#' options. Default is `"all"`.
#' @param additional_flags (character) an optional **named** character vector
#' where the values correspond to additional logical flag columns in `occ` and
#' the names are human-readable labels used when saving flagged records
#' (e.g., `c("Custom flag" = "custom_flag")`). Default is `NULL`.
#' @param force_keep (character) an optional vector with the IDs of records
#' that were flagged (`FALSE`) but should still be kept. Default is `NULL`.
#' @param force_remove (character) an optional vector with the IDs of records
#' that were not flagged (`TRUE`) but should still be removed. Default is `NULL`.
#' @param remove_NA (logical) whether to remove records that have `NA` in the
#' specified flags. Default is `FALSE`.
#' @param column_id (character) the name of the column containing unique record
#' IDs. Required if `force_keep` or `force_remove` is used. Default is
#' `"record_id"`.
#' @param save_flagged (logical) whether to save the flagged (removed) records.
#' If `TRUE`, an `output_dir` must be provided. Default is `FALSE`.
#' @param output_dir (character) path to an existing directory where removed
#' flagged records will be saved. Only used when `save_flagged = TRUE`.
#' @param overwrite (logical) whether to overwrite existing files in
#' `output_dir`. Only used when `save_flagged = TRUE`. Default is `FALSE`.
#' @param output_format (character) output format for saving removed records.
#' Options are `".csv"` or `".gz"`. Only used when `save_flagged = TRUE`.
#' Default is `".gz"`.
#'
#' @details
#' The following built-in flags are recognized when `flags = "all"` (or can be
#' passed individually without the `_flag` suffix): `correct_country`,
#' `correct_state`, `year`, `duplicated`, `fossil`, `cultivated`,
#' `inaturalist`, `florabr`, `faunabr`, `wcvp`, `iucn`, `bien`, `thin_env`,
#' `thin_geo`, `consensus`, as well as `CoordinateCleaner` flags (`.val`,
#' `.equ`, `.zer`, `.cap`, `.cen`, `.sea`, `.urb`, `.otl`, `.gbf`, `.inst`,
#' and `.aohi`).
#'
#' @returns
#' An object of the same class as `occ` (`data.frame` or `data.table`)
#' containing only the valid (kept) records according to the flags and
#' additional criteria.
#'
#' @importFrom stats setNames
#' @importFrom data.table fwrite is.data.table as.data.table
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occ_flagged", package = "RuHere")
#'
#' # Remove all flagged records
#' occ_valid <- remove_flagged(occ = occ_flagged)
#'
#' # Remove flagged records and force removal of some unflagged records
#' to_remove <- c("gbif_5987", "specieslink_2301", "gbif_18761")
#' occ_valid2 <- remove_flagged(occ = occ_flagged,
#'                              force_remove = to_remove)
#'
#' # Remove flagged records but keep some flagged ones
#' to_keep <- c("gbif_14501", "gbif_12002", "gbif_5168")
#' occ_valid3 <- remove_flagged(occ = occ_flagged,
#'                              force_keep = to_keep)
remove_flagged <- function(occ,
                           flags = "all",
                           additional_flags = NULL,
                           force_keep = NULL,
                           force_remove = NULL,
                           remove_NA = FALSE,
                           column_id = "record_id",
                           save_flagged = FALSE,
                           output_dir = NULL,
                           overwrite = FALSE,
                           output_format = ".gz") {
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

  was_dt <- data.table::is.data.table(occ)
  df <- if (was_dt) as.data.frame(occ) else occ

  # 2. Check flags
  if (!is.null(flags)) {
    if (!is.character(flags) || length(flags) == 0L || any(is.na(flags))) {
      stop("'flags' must be a non-empty character vector without NAs or NULL.", call. = FALSE)
    }

    is_all <- "all" %in% flags
    if (is_all) {
      flags <- c("correct_country", "correct_state",
                 "year", "duplicated", "fossil", "cultivated", "inaturalist",
                 "florabr", "faunabr", "wcvp", "iucn", "bien",
                 "thin_env", "thin_geo", "consensus",
                 # From CoordinateCleaner
                 ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
                 ".gbf", ".inst", ".aohi")
    }

    to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                  "fossil", "year", "inaturalist", "duplicated",
                  "thin_env", "thin_geo", "consensus")
    flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

    if (is_all) {
      flags <- intersect(flags, names(df))
    } else {
      missing_flags <- setdiff(flags, names(df))
      if (length(missing_flags) > 0L) {
        stop("The following flags do not exist in 'occ': ",
             paste(missing_flags, collapse = ", "), call. = FALSE)
      }
      flags <- unique(flags)
    }
  } else {
    flags <- character(0L)
  }

  # 3. Check additional_flags (named vector: names = labels, values = columns in occ)
  if (!is.null(additional_flags)) {
    if (!is.character(additional_flags) || length(additional_flags) == 0L || any(is.na(additional_flags))) {
      stop("'additional_flags' must be a non-empty named character vector or NULL.", call. = FALSE)
    }
    extra_labels <- names(additional_flags)
    if (is.null(extra_labels) || any(!nzchar(trimws(extra_labels)))) {
      stop("'additional_flags' must be a named character vector (e.g., c('User flag' = 'user_flag')).",
           call. = FALSE)
    }
    extra_cols <- unname(additional_flags)
    missing_extra <- setdiff(extra_cols, names(df))
    if (length(missing_extra) > 0L) {
      stop("These additional flag columns do not exist in 'occ': ",
           paste(missing_extra, collapse = ", "), call. = FALSE)
    }
    non_logical <- extra_cols[!vapply(df[, extra_cols, drop = FALSE], is.logical, logical(1L))]
    if (length(non_logical) > 0L) {
      stop("These additional flag columns are not logical: ",
           paste(non_logical, collapse = ", "), call. = FALSE)
    }
  }

  # 4. Check force_keep, force_remove & column_id
  if (!is.null(force_keep) && (!is.atomic(force_keep) || length(force_keep) == 0L)) {
    stop("'force_keep' must be a non-empty vector or NULL.", call. = FALSE)
  }
  if (!is.null(force_remove) && (!is.atomic(force_remove) || length(force_remove) == 0L)) {
    stop("'force_remove' must be a non-empty vector or NULL.", call. = FALSE)
  }

  if (!is.null(force_keep) || !is.null(force_remove)) {
    if (!is_single_str(column_id)) {
      stop("You must specify a valid 'column_id' when using 'force_keep' or 'force_remove'.",
           call. = FALSE)
    }
    if (!column_id %in% names(df)) {
      stop("Column '", column_id, "' specified in 'column_id' does not exist in 'occ'.",
           call. = FALSE)
    }
  }

  # 5. Check remove_NA & saving options
  if (!is_single_bool(remove_NA)) {
    stop("'remove_NA' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(save_flagged)) {
    stop("'save_flagged' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  if (save_flagged) {
    if (!is_single_str(output_dir)) {
      stop("You must provide a valid 'output_dir' when 'save_flagged = TRUE'.", call. = FALSE)
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

  # ---- PROCESSING FLAGS ----
  flag_names <- getExportedValue("RuHere", "flag_names")

  if (!is.null(additional_flags)) {
    extra_mapped <- stats::setNames(names(additional_flags), unname(additional_flags))
    flag_names <- c(flag_names, extra_mapped)
    flags <- unique(c(flags, unname(additional_flags)))
  }

  # Add manual force_remove flag if provided
  has_force_remove <- !is.null(force_remove)
  if (has_force_remove) {
    df$.force_remove_flag <- !(df[[column_id]] %in% force_remove)
    flags <- c(flags, ".force_remove_flag")
    flag_names <- c(flag_names, stats::setNames("force_remove", ".force_remove_flag"))
  }

  if (length(flags) == 0L) {
    warning("No flag columns were found in 'occ'. Returning original dataset.",
            call. = FALSE)
    return(occ)
  }

  # Separate records forced to be kept (preserving row indices)
  if (!is.null(force_keep)) {
    keep_mask <- df[[column_id]] %in% force_keep
    if (!any(keep_mask)) {
      warning("None of the IDs provided in 'force_keep' were found in the dataset.\n",
              "Keeping only unflagged records.", call. = FALSE)
    }
    occ_keep <- df[keep_mask, , drop = FALSE]
    df <- df[!keep_mask, , drop = FALSE]
  } else {
    occ_keep <- NULL
  }

  # Resolve NAs in flag columns (using drop = FALSE to support single-flag filtering)
  flag_mat <- as.matrix(df[, flags, drop = FALSE])
  flag_mat[is.na(flag_mat)] <- !remove_NA

  # Save flagged records per category only when save_flagged = TRUE
  if (save_flagged && nrow(df) > 0L) {
    occ_flagged <- list()
    for (col_f in flags) {
      bad_rows <- !flag_mat[, col_f]
      if (any(bad_rows)) {
        sub_bad <- df[bad_rows, , drop = FALSE]
        if (".force_remove_flag" %in% names(sub_bad)) {
          sub_bad$.force_remove_flag <- NULL
        }
        occ_flagged[[col_f]] <- sub_bad
      }
    }

    if (length(occ_flagged) > 0L) {
      match_flags <- flag_names[match(names(occ_flagged), names(flag_names))]
      na_names <- is.na(match_flags)
      match_flags[na_names] <- names(occ_flagged)[na_names]
      names(occ_flagged) <- match_flags

      saved_flag_names <- names(occ_flagged)
      paths <- file.path(output_dir, paste0(saved_flag_names, output_format))

      if (!overwrite) {
        f_exists <- file.exists(paths)
        if (any(f_exists)) {
          stop("Some flagged occurrences already exist in '", output_dir, "'.\n",
               "Delete the file, change the folder or set 'overwrite = TRUE'.",
               call. = FALSE)
        }
      }

      for (y in seq_along(saved_flag_names)) {
        data.table::fwrite(x = occ_flagged[[saved_flag_names[y]]],
                           file = paths[y])
      }
    }
  }

  # Keep records passing all flags
  valid_rows <- rowSums(flag_mat) == ncol(flag_mat)
  occ_final <- df[valid_rows, , drop = FALSE]

  # Restore force_keep records
  if (!is.null(occ_keep) && nrow(occ_keep) > 0L) {
    occ_final <- rbind(occ_final, occ_keep)
  }

  # Clean up temporary helper column
  if (".force_remove_flag" %in% names(occ_final)) {
    occ_final$.force_remove_flag <- NULL
  }

  if (was_dt) {
    return(data.table::as.data.table(occ_final))
  }
  return(occ_final)
}
