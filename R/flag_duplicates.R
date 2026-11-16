#' Flag duplicated records
#'
#' @description
#' This function identifies duplicated records based on species name and
#' coordinates, as well as user-defined additional columns or raster cells.
#' Among duplicated records, the function keeps only one unflagged record,
#' chosen according to a continuous variable (e.g., keeping the most recent),
#' a categorical variable (e.g., prioritizing a specific data source), or
#' randomly.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be examined, preferably standardized using `format_columns()`.
#' Must contain the columns specified in `species`, `long`, and `lat` arguments.
#' @param species (character) the name of the column containing species names.
#' Default is `"species"`.
#' @param long (character) the name of the column containing longitude values.
#' Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column containing latitude values.
#' Default is `"decimalLatitude"`.
#' @param additional_groups (character) optional vector of additional column
#' names to consider when identifying duplicates. For example, if `"year"` is
#' included, records with the same coordinates but different collection years
#' will not be flagged. Default is `NULL`.
#' @param continuous_variable (character) optional name of a numeric column used
#' to sort duplicated records and select one to remain unflagged. Default is
#' `NULL`.
#' @param decreasing (logical) whether to sort records in decreasing order using
#' the `continuous_variable` (e.g., from most recent to oldest when the variable
#' is `"year"`). Only applicable when `continuous_variable` is not `NULL`.
#' Default is `TRUE`.
#' @param categorical_variable (character) optional name of a categorical column
#' used to sort duplicated records and select one to remain unflagged. If
#' provided, the order of priority must be specified through
#' `priority_categories`. Default is `NULL`.
#' @param priority_categories (character) vector of categories, in the desired
#' order of priority, present in the column specified in `categorical_variable`.
#' Required when `categorical_variable` is not `NULL`. Default is `NULL`.
#' @param by_cell (logical) whether to use raster cells instead of raw
#' coordinates to identify duplicates (i.e., all records inside the same raster
#' cell are treated as duplicates). If `TRUE`, a `SpatRaster` must be supplied
#' in `raster_variable`. Default is `FALSE`.
#' @param raster_variable (SpatRaster) a `SpatRaster` used to identify
#' duplicated records by raster cell. Only applicable when `by_cell` is `TRUE`.
#' Default is `NULL`.
#' @param keep_cell_id (logical) whether to retain the `cell_id` column in the
#' returned object when `by_cell = TRUE`. Default is `FALSE`.
#'
#' @returns
#' An object of the same class as `occ` (`data.frame` or `data.table`) augmented
#' with a new logical column named `duplicated_flag` (and optionally `cell_id`
#' if `by_cell = TRUE` and `keep_cell_id = TRUE`). Records identified as
#' duplicated receive `FALSE`, while unique/retained records receive `TRUE`.
#' As with all other flagging functions in RuHere, `TRUE` indicates that the
#' record passed this test and is eligible for retention; `FALSE` indicates it
#' failed and is flagged as potentially problematic, and can be removed using
#' `remove_flagged()`.
#'
#' @export
#'
#' @importFrom terra cellFromXY
#' @importFrom data.table is.data.table as.data.table copy set setorderv fifelse
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Duplicate some records as example
#' occurrences <- rbind(occurrences[1:1000, ], occurrences[1:100, ])
#' # Flag duplicates
#' occ_dup <- flag_duplicates(occ = occurrences)
#' sum(!occ_dup$duplicated_flag) # Number of duplicated records
flag_duplicates <- function(occ,
                            species = "species",
                            long = "decimalLongitude",
                            lat = "decimalLatitude",
                            additional_groups = NULL,
                            continuous_variable = NULL,
                            decreasing = TRUE,
                            categorical_variable = NULL,
                            priority_categories = NULL,
                            by_cell = FALSE,
                            raster_variable = NULL,
                            keep_cell_id = FALSE) {

  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  is_single_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  # 1. Check occ
  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be provided (cannot be NULL or missing).", call. = FALSE)
  }
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table, not ",
         paste(class(occ), collapse = "/"), ".", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # 2. Check species, long, lat
  if (!is_single_str(species) || !species %in% names(occ)) {
    stop("The column specified in 'species' ('", species, "') was not found in 'occ'.",
         call. = FALSE)
  }
  if (!is_single_str(long) || !long %in% names(occ)) {
    stop("The column specified in 'long' ('", long, "') was not found in 'occ'.",
         call. = FALSE)
  }
  if (!is_single_str(lat) || !lat %in% names(occ)) {
    stop("The column specified in 'lat' ('", lat, "') was not found in 'occ'.",
         call. = FALSE)
  }
  if (!is.numeric(occ[[long]]) || !is.numeric(occ[[lat]])) {
    stop("'long' and 'lat' columns must be numeric.", call. = FALSE)
  }

  # 3. Check additional_groups
  if (!is.null(additional_groups)) {
    if (!is.character(additional_groups) || length(additional_groups) == 0L || any(is.na(additional_groups))) {
      stop("'additional_groups' must be a character vector without NAs or NULL.", call. = FALSE)
    }
    missing_ag <- setdiff(additional_groups, names(occ))
    if (length(missing_ag) > 0L) {
      stop("The following columns specified in 'additional_groups' were not found in 'occ': ",
           paste(missing_ag, collapse = ", "), call. = FALSE)
    }
  }

  # 4. Check continuous_variable & decreasing
  if (!is.null(continuous_variable)) {
    if (!is_single_str(continuous_variable) || !continuous_variable %in% names(occ)) {
      stop("The column specified in 'continuous_variable' ('", continuous_variable,
           "') was not found in 'occ'.", call. = FALSE)
    }
    if (!is.numeric(occ[[continuous_variable]])) {
      stop("The column specified in 'continuous_variable' ('", continuous_variable,
           "') must be numeric or integer.", call. = FALSE)
    }
  }
  if (!is_single_bool(decreasing)) {
    stop("'decreasing' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # 5. Check categorical_variable & priority_categories
  if (!is.null(categorical_variable)) {
    if (!is_single_str(categorical_variable) || !categorical_variable %in% names(occ)) {
      stop("The column specified in 'categorical_variable' ('", categorical_variable,
           "') was not found in 'occ'.", call. = FALSE)
    }
    if (is.null(priority_categories) || !is.character(priority_categories) ||
        length(priority_categories) == 0L || any(is.na(priority_categories))) {
      stop("'priority_categories' must be a non-empty character vector when 'categorical_variable' is provided.",
           call. = FALSE)
    }
    present_cats <- unique(as.character(occ[[categorical_variable]]))
    missing_pcats <- setdiff(priority_categories, present_cats)
    if (length(missing_pcats) > 0L) {
      stop("The following values in 'priority_categories' are not present in column '",
           categorical_variable, "': ", paste(missing_pcats, collapse = ", "),
           call. = FALSE)
    }
  } else if (!is.null(priority_categories) && !is.character(priority_categories)) {
    stop("'priority_categories' must be a character vector or NULL.", call. = FALSE)
  }

  # 6. Check by_cell, raster_variable & keep_cell_id
  if (!is_single_bool(by_cell)) {
    stop("'by_cell' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(keep_cell_id)) {
    stop("'keep_cell_id' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (by_cell) {
    if (is.null(raster_variable)) {
      stop("'raster_variable' must be provided when 'by_cell' is TRUE.", call. = FALSE)
    }
    if (!inherits(raster_variable, "SpatRaster")) {
      stop("'raster_variable' must be a SpatRaster, not ",
           paste(class(raster_variable), collapse = "/"), ".", call. = FALSE)
    }
  }

  # ---- PREPARE WORKING DATA.TABLE ----
  was_dt <- data.table::is.data.table(occ)
  n_rows <- nrow(occ)

  # Build lightweight working table with only required columns to avoid sorting the entire dataset
  needed_cols <- unique(c(species, long, lat, additional_groups,
                          continuous_variable, categorical_variable))
  work <- data.table::as.data.table(
    if (was_dt) occ[, needed_cols, with = FALSE] else occ[, needed_cols, drop = FALSE]
  )
  data.table::set(work, j = "..orig_row..", value = seq_len(n_rows))

  # Compute raster cell IDs fast using cellFromXY (no pixel value extraction)
  if (by_cell) {
    xy_mat <- cbind(work[[long]], work[[lat]])
    cell_ids <- terra::cellFromXY(raster_variable[[1]], xy_mat)
    data.table::set(work, j = "cell_id", value = cell_ids)
    to_group <- c(species, "cell_id", additional_groups)
    valid_spatial <- !is.na(work[[species]]) & !is.na(cell_ids)
  } else {
    to_group <- c(species, long, lat, additional_groups)
    valid_spatial <- !is.na(work[[species]]) & !is.na(work[[long]]) & !is.na(work[[lat]])
  }

  # ---- ORDERING LOGIC (NA LAST) ----
  # Categorical priority takes precedence if both are provided, or apply both sequentially
  if (!is.null(continuous_variable)) {
    data.table::setorderv(
      work,
      cols = continuous_variable,
      order = if (decreasing) -1L else 1L,
      na.last = TRUE
    )
  }

  if (!is.null(categorical_variable)) {
    cat_vals <- as.character(work[[categorical_variable]])
    cat_rank <- match(cat_vals, priority_categories)
    cat_rank[is.na(cat_rank)] <- length(priority_categories) + 1L
    data.table::set(work, j = "..cat_rank..", value = cat_rank)
    data.table::setorderv(work, cols = "..cat_rank..", order = 1L, na.last = TRUE)
  }

  # If neither continuous nor categorical variable is provided, break ties randomly
  if (is.null(continuous_variable) && is.null(categorical_variable)) {
    work <- work[sample.int(n_rows)]
  }

  # ---- FLAG DUPLICATES ----
  # Only evaluate valid spatial records so NA coordinates/cells are not falsely grouped as duplicates
  valid_sub <- work[valid_spatial]
  keep_in_valid <- !duplicated(valid_sub, by = to_group)

  dup_flag <- rep(TRUE, n_rows)
  dup_flag[valid_sub[["..orig_row.."]][!keep_in_valid]] <- FALSE

  # ---- RETURN ORIGINAL OBJECT STRUCTURE & ROW ORDER ----
  if (was_dt) {
    out <- data.table::copy(occ)
    if (by_cell && keep_cell_id) {
      data.table::set(out, j = "cell_id", value = cell_ids)
    }
    data.table::set(out, j = "duplicated_flag", value = dup_flag)
  } else {
    out <- occ
    if (by_cell && keep_cell_id) {
      out$cell_id <- cell_ids
    }
    out$duplicated_flag <- dup_flag
  }

  return(out)
}
