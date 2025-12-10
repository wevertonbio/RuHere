#' Flag records that are close to each other in the enviromnetal space
#'
#' @description
#' Flags occurrence records for thinning by keeping only one record per species
#' within the same environmental block/bin.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records. Must contain columns for species, longitude, and latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param env_layers (SpatRaster) object containing environmental variables.
#' @param n_bins (numeric) number of bins into which each environmental variable
#' will be divided.
#' @param prioritary_column (character) name of a numeric columns in `occ`to
#' define retention priority (e.g., quality score, year). See details.
#' @param decreasing (logical) whether to sort records in decreasing order using
#' the `prioritary_column` (e.g., from most recent to oldest when the variable
#' is `"year"`). Only applicable when `prioritary_column` is not `NULL`.
#' Default is `TRUE`.
#' @param flag_for_NA (logical) whether to treat records falling in `NA` cells
#' of `env_layers` as valid (`TRUE`) or invalid (`FALSE`). Default is `FALSE`.
#'
#' @details
#' This function used `get_env_bins()` to create a multidimensional grid in
#' environmental space by splitting each environmental variable into `n_bins`
#' equally sized intervals. Records falling into the same environmental bin are
#' considered redundant; only one is kept (based on retention priority when
#' provided), and the remaining records are flagged.
#'
#'
#' @returns
#' The original `occ` data frame with two additional columns:
#' * `thin_env_flag`: logical indicating whether each record is retained
#'   (`TRUE`) or flagged as redundant (`FALSE`).
#' * `bin`: environmental bin ID assigned to each record. Each component
#'   of the ID corresponds to the bin of one environmental variable.
#'
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Get only occurrences from Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Load example of raster variables
#' data("worldclim", package = "RuHere")
#' # Unwrap Packed raster
#' r <- terra::unwrap(worldclim)
#' # Flag records that are close to each other in the enviromnetal space
#' occ_env_thin <- thin_env(occ = occ, env_layers = r)
#' # Number of flagged (redundant) records
#' sum(!occ_env_thin$thin_env_flag) #Number of flagged records
thin_env <- function(occ, species = "species",
                     long = "decimalLongitude",
                     lat = "decimalLatitude",
                     env_layers, n_bins = 5, prioritary_column = NULL,
                     decreasing = TRUE, flag_for_NA = FALSE){
  #==============================#
  #      ARGUMENT CHECKING       #
  #==============================#

  #--- occ must be a data.frame ---
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table")
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  #--- Required column names ---
  required_cols <- c(species, long, lat)

  missing_cols <- required_cols[!required_cols %in% names(occ)]
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from 'occ': ",
         paste(missing_cols, collapse = ", "))
  }

  #--- Coordinates must be numeric ---
  if (!is.numeric(occ[[long]]) || !is.numeric(occ[[lat]])) {
    stop("Longitude and latitude columns must be numeric")
  }

  # env_layers
  if (!inherits(env_layers, "SpatRaster")) {
    stop("'env_layers' must be a SpatRaster object")
  }
  if (terra::nlyr(env_layers) < 1) {
    stop("'env_layers' must contain at least one layer")
  }

  #--- d must be valid numeric > 0 ---
  if (!is.numeric(n_bins) || length(n_bins) != 1 || is.na(n_bins)) {
    stop("'n_bins' must be a single numeric value")
  }
  if (n_bins <= 0) {
    stop("'n_bins' must be a positive number")
  }

  #--- prioritary_column checks ---
  if (!is.null(prioritary_column)) {

    if (!is.character(prioritary_column) || length(prioritary_column) != 1) {
      stop("'prioritary_column' must be a single character string")
    }

    if (!prioritary_column %in% names(occ)) {
      stop("The column defined in 'prioritary_column' was not found in 'occ'")
    }

    # Must be numeric for ordering
    if (!is.numeric(occ[[prioritary_column]])) {
      stop("'prioritary_column' must refer to a numeric column in 'occ'")
    }

    # decreasing must be logical
    if (!is.logical(decreasing) || length(decreasing) != 1) {
      stop("'decreasing' must be TRUE or FALSE")
    }
  }

  #--- flag_for_NA must be logical ---
  if (!is.logical(flag_for_NA) || length(flag_for_NA) != 1) {
    stop("'flag_for_NA' must be TRUE or FALSE")
  }

  # Split by species
  spp <- unique(occ[[species]])

  res <- lapply(spp, function(x){
    occ_x <- occ[occ[[species]] == x, ]

    # Add an original ID for final sorting and merging
    occ_x$original_id <- 1:nrow(occ_x)

    # Get bins
    b <- get_env_bins(occ = occ_x, species = species, long = long, lat = lat,
                      env_layers = env_layers, n_bins = n_bins)

    # Append results
    occ_x$bin <- b$data$block_id
    # Get NA
    if(anyNA(occ_x$bin)){
      occ_na <- occ_x[is.na(occ_x$bin), ]
      occ_x <- occ_x[!is.na(occ_x$bin), ]
      # Which flag add in NA?
      if(flag_for_NA){
        occ_na$thin_env_flag[is.na(occ_na$bin)] <- TRUE
      } else {
        occ_na$thin_env_flag[is.na(occ_na$bin)] <- FALSE
      }
    } else {
      occ_na <- data.frame()
    }

    # Flag duplicates
    if(is.null(prioritary_column)){
      groups <- split(seq_len(nrow(occ_x)), occ_x["bin"], drop = TRUE)
      occ_x$thin_env_flag <- FALSE
      occ_x$thin_env_flag[ sapply(groups, `[`, 1) ] <- TRUE
    }

    if(!is.null(prioritary_column)){
      ord <- order(occ_x[[prioritary_column]], decreasing = decreasing)
      occ_x <- occ_x[ord, ]
      grp <- interaction(occ_x["bin"], drop = TRUE)
      occ_x$thin_env_flag <- !duplicated(grp)
      occ_x <- occ_x[order(ord), ]
    }

    if(nrow(occ_na) > 0){
      occ_x <- rbind(occ_x, occ_na)
    }

    #Sor for original id
    occ_x <- occ_x[order(occ_x$original_id),]
    occ_x$original_id <- NULL
    return(occ_x)
  })
  return(as.data.frame(data.table::rbindlist(res)))
}
