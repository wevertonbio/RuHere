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
#' @param occ (data.frame) a data frame containing the occurrence records to be
#' examined, preferably standardized using `format_columns()`. Must contain the
#' columns specified in `species`, `long` and `lat` arguments.
#' @param species (character) the name of the column containing species names.
#' Default is "species".
#' @param long (character)  the name of the column containing longitude values.
#' Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column containing latitude values.
#' Default is `"decimalLatitude"`.
#' @param additional_groups (character) optional vector of additional column
#' names to consider when identifying duplicates. For example, if `"year"` is
#' included, records with the same coordinates but different collection years
#' will not be flagged. Default is `NULL`.
#' @param continuous_variable (character) optional name of a numeric column used
#' to sort duplicated records and select one to remain unflagged. Default is
#' `NULL`, meaning that no sorting will occur and the unflagged record will be
#' selected randomly.
#' @param decreasing (logical) whether to sort records in decreasing order using
#' the `continuous_variable` (e.g., from most recent to oldest when the variable
#' is `"year"`). Only applicable when `continuous_variable` is not `NULL`.
#' Default is `TRUE`.
#' @param categorical_variable (character) (character) optional name of a
#' categorical column used to sort duplicated records and select one to remain
#' unflagged. If provided, the order of priority must be specified through
#' `priority_categories`. Default is `NULL`.
#' @param priority_categories (character) vector of categories, in the desired
#' order of priority, present in the column specified in `categorical_variable`.
#' Only applicable when `categorical_variable` is not `NULL`. Default is `NULL`.
#' @param by_cell (logical) whether to use raster cells instead of raw
#' coordinates to identify duplicates (i.e., all records inside the same raster
#' cell are treated as duplicates). If `TRUE`, a `SpatRaster` must be supplied
#' in `raster_variable`. Default is `FALSE`.
#' @param raster_variable (SpatRaster) a `SpatRaster` used to identify
#' duplicated records by raster cell. Only applicable when `by_cell` is `TRUE`.
#' Default is `NULL`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame augmented with
#' a new column named \code{duplicated_flag}. Records identified as duplicated
#' receive \code{FALSE}, while all unique retained records receive \code{TRUE}.
#'
#' @export
#'
#' @importFrom terra extract
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Duplicate some records as example
#' occurrences <- rbind(occurrences[1:1000, ], occurrences[1:100,])
#' # Flag duplicates
#' occ_dup <- flag_duplicates(occ = occurrences)
#' sum(!occ_dup$duplicated_flag) #Number of duplicated records
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
                            raster_variable = NULL){

  #       ARGUMENT CHECKING       #
  # ----------------------------- #

  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be provided (cannot be NULL or missing).")
  } else if (!inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame, not ", class(occ))
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # basic column names (species, long, lat) must be character and exist in occ
  if (!inherits(species, "character")) {
    stop("'species' must be a character, not ", class(species))
  }
  if (!species %in% names(occ)) {
    stop("The column specified in 'species' ('", species, "') was not found in 'occ'.")
  }

  if (!inherits(long, "character")) {
    stop("'long' must be a character, not ", class(long))
  }
  if (!long %in% names(occ)) {
    stop("The column specified in 'long' ('", long, "') was not found in 'occ'.")
  }

  if (!inherits(lat, "character")) {
    stop("'lat' must be a character, not ", class(lat))
  }
  if (!lat %in% names(occ)) {
    stop("The column specified in 'lat' ('", lat, "') was not found in 'occ'.")
  }

  # additional_groups: if provided, must be character and all columns must exist
  if (!is.null(additional_groups) && !inherits(additional_groups, "character")) {
    stop("'additional_groups' must be a character vector or NULL, not ", class(additional_groups))
  }
  if (!is.null(additional_groups)) {
    missing_ag <- additional_groups[!additional_groups %in% names(occ)]
    if (length(missing_ag) > 0) {
      stop("The following columns specified in 'additional_groups' were not found in 'occ': ",
           paste(missing_ag, collapse = ", "))
    }
  }

  # continuous_variable: if provided, must be character, present in occ, and numeric
  if (!is.null(continuous_variable) &&
      !inherits(continuous_variable, "character")) {
    stop("'continuous_variable' must be a character or NULL, not ", class(continuous_variable))
  }
  if (!is.null(continuous_variable)) {
    if (!continuous_variable %in% names(occ)) {
      stop("The column specified in 'continuous_variable' ('", continuous_variable, "') was not found in 'occ'.")
    }
    if (!inherits(occ[[continuous_variable]], "numeric")) {
      stop("The column specified in 'continuous_variable' ('", continuous_variable, "') must be numeric.")
    }
  }

  # decreasing must be logical
  if (!inherits(decreasing, "logical")) {
    stop("'decreasing' must be logical, not ", class(decreasing))
  }

  # categorical_variable & priority_categories checks
  if (!is.null(categorical_variable) && !inherits(categorical_variable, "character")) {
    stop("'categorical_variable' must be a character or NULL, not ", class(categorical_variable))
  }
  if (!is.null(categorical_variable)) {
    if (!categorical_variable %in% names(occ)) {
      stop("The column specified in 'categorical_variable' ('", categorical_variable, "') was not found in 'occ'.")
    }
  }
  if (!is.null(priority_categories) && !inherits(priority_categories, "character")) {
    stop("'priority_categories' must be a character vector or NULL, not ", class(priority_categories))
  }
  if (!is.null(categorical_variable) && !is.null(priority_categories)) {
    # check that priority categories exist in the categorical column
    present_cats <- unique(as.character(occ[[categorical_variable]]))
    missing_pcats <- setdiff(priority_categories, present_cats)
    if (length(missing_pcats) > 0) {
      stop("The following values in 'priority_categories' are not present in column '",
           categorical_variable, "': ", paste(missing_pcats, collapse = ", "))
    }
  }

  # by_cell & raster_variable checks
  if (!inherits(by_cell, "logical")) {
    stop("'by_cell' must be logical, not ", class(by_cell))
  }
  if (by_cell) {
    if (is.null(raster_variable)) {
      stop("'raster_variable' must be provided when 'by_cell' is TRUE.")
    }
    if (!inherits(raster_variable, "SpatRaster")) {
      stop("'raster_variable' must be a SpatRaster, not ", class(raster_variable))
    }
  }


  # Extract ID of rasters if by_cell
  if(by_cell){

    if(is.null(raster_variable)){
      stop("If 'by_cell = TRUE', 'raster_variable' must be provided")
    }

    if(!inherits(raster_variable, "SpatRaster")){
      stop("If 'by_cell = TRUE', raster_variable must be a 'SpatRaster', not a ",
           class(raster_variable))
    }

    occ$cell_id <- terra::extract(raster_variable[[1]],
                              occ[, c(long, lat)], cells = TRUE)[["cell"]]

    to_group <- c(species, "cell_id")
  } else {
    to_group <- c(species, long, lat)
  }

  if(!is.null(additional_groups)){
    to_group <- c(to_group, additional_groups)
  }

  # Check NAs
  na_rows <- which(!stats::complete.cases(occ[to_group]))
  if(length(na_rows) > 0){
    na_df <- occ[na_rows, ]
    na_df$duplicated_flag <- FALSE
    occ <- occ[-na_rows, ]
  }

  if(is.null(continuous_variable) & is.null(categorical_variable)){
    groups <- split(seq_len(nrow(occ)), occ[to_group], drop = TRUE)
    occ$duplicated_flag <- FALSE
    occ$duplicated_flag[ sapply(groups, `[`, 1) ] <- TRUE
    }

  if(!is.null(continuous_variable) & is.null(categorical_variable)){
    ord <- order(occ[[continuous_variable]], decreasing = decreasing)
    occ <- occ[ord, ]
    grp <- interaction(occ[to_group], drop = TRUE)
    occ$duplicated_flag <- !duplicated(grp)
    occ <- occ[order(ord), ]
    }

  if(!is.null(categorical_variable) & is.null(continuous_variable)){
    ord <- order(match(occ[[categorical_variable]], priority_categories))
    occ <- occ[ord, ]
    grp <- interaction(occ[to_group], drop = TRUE)
    occ$duplicated_flag <- !duplicated(grp)
    occ <- occ[order(ord), ]
  }

  if(!is.null(categorical_variable) & !is.null(continuous_variable)){
    ord <- order(
      if (decreasing) -occ[[continuous_variable]] else occ[[continuous_variable]],
      match(occ[[categorical_variable]], priority_categories)
    )
    occ <- occ[ord, ]
    grp <- interaction(occ[to_group], drop = TRUE)
    occ$duplicated_flag <- !duplicated(grp)

  }

  if(length(na_rows) > 0){
   occ <- rbind(occ, na_df)
  }

  return(occ)
}
