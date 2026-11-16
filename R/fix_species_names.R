#' Standardize or fix species names using a dictionary
#'
#' @description
#' Replaces misspelled, outdated, or unstandardized species names in an
#' occurrence dataset using a user-supplied reference dictionary.
#'
#' @param data (data.frame or data.table) a dataset containing species names,
#' preferably standardized using `format_columns()`.
#' @param species (character) the name of the column in `data` containing the
#' species names to be checked and replaced. Default is `"species"`.
#' @param dictionary (data.frame or data.table) a reference table mapping
#' original names to standardized/corrected names.
#' @param original_name (character) the name of the column in `dictionary`
#' containing the original (input) species names. Default is `"input_name"`.
#' @param fixed_name (character) the name of the column in `dictionary`
#' containing the corrected/suggested species names. Default is
#' `"Suggested_name"`.
#' @param old_names (logical) whether to retain the original species names in a
#' new column named `"old_names"` before replacing them. Default is `FALSE`.
#' @param flag_col (character) the name of the logical column appended to `data`
#' indicating whether each record's species name was modified (`TRUE`) or kept
#' unchanged (`FALSE`). Default is `"fixed_name"`.
#'
#' @details
#' The function performs an exact hash-based lookup (`match()`) between the
#' species column in `data` and `original_name` in `dictionary`. If `dictionary`
#' contains exact duplicate pairs (`original_name` -> `fixed_name`), duplicates
#' are silently collapsed. However, if the same `original_name` maps to
#' conflicting values in `fixed_name`, the function throws an informative error
#' listing the conflicting names so they can be resolved beforehand.
#'
#' Records whose names match an entry in `dictionary` with a valid, non-empty,
#' and distinct `fixed_name` are updated in place, and the `flag_col` column is
#' set to `TRUE`. Records with `NA` in `fixed_name` or identical matches are
#' left untouched (`FALSE` in `flag_col`).
#'
#' @returns
#' An object of the same class as `data` (`data.frame` or `data.table`) with
#' updated species names in the `species` column, a logical flag column
#' (`flag_col`), and optionally an `"old_names"` column if `old_names = TRUE`.
#'
#' @importFrom data.table is.data.table as.data.table copy set
#' @importFrom utils head
#'
#' @export
#'
#' @examples
#' # Create a sample occurrence dataset with typos and synonyms
#' occ <- data.frame(
#'   species = c(
#'     "Araucaria angustifolia",
#'     "Araucaria angustiflia",   # typo
#'     "Euterpe edulis",
#'     "Euterpe edullis",         # typo
#'     "Cedrela glaziovii",       # synonym
#'     "Handroanthus albus"       # not in dictionary (kept unchanged)
#'   ),
#'   decimalLongitude = c(-49.2, -50.1, -48.5, -47.9, -49.0, -49.3),
#'   decimalLatitude  = c(-25.4, -26.2, -25.1, -24.8, -25.6, -16.7),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create a reference dictionary mapping input names to accepted names
#' dict <- data.frame(
#'   input_name = c(
#'     "Araucaria angustifolia",
#'     "Araucaria angustiflia",
#'     "Euterpe edullis",
#'     "Cedrela glaziovii"
#'   ),
#'   Suggested_name = c(
#'     "Araucaria angustifolia",  # identical (will not be flagged as changed)
#'     "Araucaria angustifolia",  # will be fixed
#'     "Euterpe edulis",          # will be fixed
#'     "Cedrela fissilis"         # will be fixed
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Fix species names and retain original names for comparison
#' occ_fixed <- fix_species_names(
#'   data = occ,
#'   species = "species",
#'   dictionary = dict,
#'   old_names = TRUE
#' )
#'
#' # Compare original vs. standardized names
#' occ_fixed[, c("old_names", "species", "fixed_name")]
fix_species_names <- function(data,
                              species = "species",
                              dictionary,
                              original_name = "input_name",
                              fixed_name = "Suggested_name",
                              old_names = FALSE,
                              flag_col = "fixed_name") {

  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }

  # 1. Check data
  if (missing(data) || !inherits(data, "data.frame")) {
    stop("'data' must be a data.frame or data.table.", call. = FALSE)
  }
  if (nrow(data) == 0L) {
    stop("'data' is empty. Please provide a dataset with occurrence records.", call. = FALSE)
  }

  # 2. Check dictionary
  if (missing(dictionary) || !inherits(dictionary, "data.frame")) {
    stop("'dictionary' must be a data.frame or data.table.", call. = FALSE)
  }
  if (nrow(dictionary) == 0L) {
    stop("'dictionary' is empty. Please provide a non-empty reference dictionary.", call. = FALSE)
  }

  # 3. Check column name arguments
  if (!is_single_str(species) || !species %in% names(data)) {
    stop(paste0("The column specified in 'species' ('", species, "') was not found in 'data'."), call. = FALSE)
  }
  if (!is_single_str(original_name) || !original_name %in% names(dictionary)) {
    stop(paste0("The column specified in 'original_name' ('", original_name, "') was not found in 'dictionary'."), call. = FALSE)
  }
  if (!is_single_str(fixed_name) || !fixed_name %in% names(dictionary)) {
    stop(paste0("The column specified in 'fixed_name' ('", fixed_name, "') was not found in 'dictionary'."), call. = FALSE)
  }

  # 4. Check old_names and flag_col
  if (!is.logical(old_names) || length(old_names) != 1L || is.na(old_names)) {
    stop("'old_names' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_str(flag_col)) {
    stop("'flag_col' must be a single non-empty character string.", call. = FALSE)
  }
  if (identical(species, flag_col)) {
    stop("'flag_col' must be different from 'species'.", call. = FALSE)
  }
  if (old_names && identical(flag_col, "old_names")) {
    stop("'flag_col' cannot be 'old_names' when 'old_names = TRUE'.", call. = FALSE)
  }

  # ---- PREPARE DATA & DICTIONARY ----

  # Avoid double memory allocation when copying
  was_dt <- data.table::is.data.table(data)
  DT <- if (was_dt) data.table::copy(data) else data.table::as.data.table(data)

  orig <- as.character(dictionary[[original_name]])
  fixd <- as.character(dictionary[[fixed_name]])

  # Treat empty strings in fixed_name as NA so valid names are not erased
  fixd[!is.na(fixd) & !nzchar(trimws(fixd))] <- NA_character_

  # Filter out NA keys in original_name
  ok   <- !is.na(orig) & nzchar(trimws(orig))
  orig <- orig[ok]
  fixd <- fixd[ok]

  # Check for conflicting duplicates in dictionary (including NA vs non-NA mismatches)
  if (anyDuplicated(orig)) {
    first <- fixd[match(orig, orig)]
    diff_val <- (is.na(fixd) != is.na(first)) | (!is.na(fixd) & !is.na(first) & fixd != first)
    bad <- unique(orig[diff_val])
    if (length(bad) > 0L) {
      stop(
        "'dictionary' maps the same name to different corrections: ",
        paste(utils::head(bad, 5L), collapse = ", "),
        if (length(bad) > 5L) ", ..." else "",
        ". Please resolve these conflicts first.",
        call. = FALSE
      )
    }
    keep <- !duplicated(orig)
    orig <- orig[keep]
    fixd <- fixd[keep]
  }

  # ---- MATCH & REPLACE ----

  sp <- DT[[species]]
  if (is.factor(sp)) {
    sp <- as.character(sp)
    data.table::set(DT, j = species, value = sp)
  } else if (!is.character(sp)) {
    stop("'species' column in 'data' must be character or factor.", call. = FALSE)
  }

  # Hash lookup
  idx <- match(sp, orig)
  new <- fixd[idx]

  corrected <- !is.na(idx) & !is.na(new) & (new != sp)

  # Store original names if requested
  if (old_names) {
    data.table::set(DT, j = "old_names", value = sp)
  }

  # Update changed names in-place
  if (any(corrected)) {
    sp[corrected] <- new[corrected]
    data.table::set(DT, j = species, value = sp)
  }

  data.table::set(DT, j = flag_col, value = corrected)

  if (was_dt) DT else as.data.frame(DT)
}
