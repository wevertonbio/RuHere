#' Flag records that are close to each other in the geographic space
#'
#' @description
#' Marks occurrence records for thinning by keeping only one record per species
#' within a radius of 'd' kilometers.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be flagged. Must contain columns for species, longitude, and
#' latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param d (numeric) thinning distance in \strong{kilometers} (e.g., 10 for
#' 10km).
#' @param prioritary_column (character) name of a numeric columns in `occ`to
#' define retention priority (e.g., quality score, year). See details.
#' @param decreasing (logical) whether to sort records in decreasing order using
#' the `prioritary_column` (e.g., from most recent to oldest when the variable
#' is `"year"`). Only applicable when `prioritary_column` is not `NULL`.
#' Default is `TRUE`.
#' @param remove_invalid (logical) whether to remove invalid coordinates.
#' Default is `TRUE`.
#' @param optimize_memory (logical) whether to compute the distance matrix
#' using a C++ implementation that reduces memory usage at the cost of
#' increased computation time. Recommended for large datasets (> 10,000 records).
#' Default is FALSE.
#' @param verbose (logical) whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently. Default
#' is TRUE.
#'
#' @details
#' This function is similar to the `thin()` function from the **spThin** package,
#' but with an important difference: it allows specifying a priority order for
#' retaining records.
#'
#' When a thinning distance is provided (e.g., 10 km), the function identifies
#' clusters of records within this distance. Within each cluster, it keeps the
#' record with the highest priority according to the column defined in
#' `prioritary_column` (for example, keeping the most recent record if
#' `prioritary_column = "year"`), and flags the remaining nearby records for
#' removal.
#'
#' If `prioritary_column` is `NULL`, the priority follows the original order of
#' rows in the input `occ` data.frame.
#'
#'
#' @return
#' The original \code{occ} data frame augmented with a new logical column named
#' \code{thin_geo_flag}. Records that are retained after thinning receive
#' \code{TRUE}, while records identified as too close to a higher-priority
#' record receive \code{FALSE}.
#'
#' @export
#'
#' @importFrom fields rdist.earth RdistEarth
#' @importFrom data.table rbindlist
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Subset occurrences for Araucaria angustifolia
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Thin records using a 10 km distance threshold
#' occ_thin <- thin_geo(occ = occ, d = 10)
#' sum(!occ_thin$thin_geo_flag)  # Number of records flagged for removal
#' # Prioritizing more recent records within each cluster
#' occ_thin_recent <- thin_geo(occ = occ, d = 10, prioritary_column = "year")
#' sum(!occ_thin_recent$thin_geo_flag)  # Number of records flagged for removal
#'
thin_geo <- function(occ,
                     species = "species",
                     long = "decimalLongitude",
                     lat = "decimalLatitude",
                     d,
                     prioritary_column = NULL,
                     decreasing = TRUE,
                     remove_invalid = TRUE,
                     optimize_memory = FALSE,
                     verbose = TRUE) {
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

  #--- d must be valid numeric > 0 ---
  if (!is.numeric(d) || length(d) != 1 || is.na(d)) {
    stop("'d' must be a single numeric value (distance in kilometers)")
  }
  if (d <= 0) {
    stop("'d' must be a positive number (distance in kilometers)")
  }

  #--- Check species column (single species only) ---
  n_spp <- length(unique(occ[[species]]))
  if (n_spp > 1) {
    stop("'occ' must contain occurrences of a SINGLE species")
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

  #--- remove_invalid must be logical ---
  if (!is.logical(remove_invalid) || length(remove_invalid) != 1) {
    stop("'remove_invalid' must be TRUE or FALSE")
  }

  #--- optimize_memory must be logical ---
  if (!is.logical(optimize_memory) || length(optimize_memory) != 1) {
    stop("'optimize_memory' must be TRUE or FALSE")
  }



  # Split by species
  spp <- unique(occ[[species]])

  res <- lapply(spp, function(x){

    # Select species i
    occ_x <- occ[occ[[species]] == x, ]

    # --- 1. Pre-processing and Prioritization ---

    # Add an original ID for final sorting and merging
    occ_x$original_id <- 1:nrow(occ_x)

    # Handle invalid coordinates
    if (remove_invalid) {
      n_all <- nrow(occ_x)
      occ_valid <- remove_invalid_coordinates(occ = occ_x,
                                              long = long, lat = lat,
                                              return_invalid = FALSE)
      if (n_all > nrow(occ_valid)) {
        if (verbose) message("Removing ", n_all - nrow(occ_valid), " invalid records")
        # Use only valid records for thinning
        occ_thin <- occ_valid
      } else {
        occ_thin <- occ_x
      }
    } else {
      occ_thin <- occ_x
    }

    # Apply prioritization sorting on the subset of valid data ('occ_thin')
    if (!is.null(prioritary_column)) {
      if (decreasing) {
        ord_index <- order(occ_thin[[prioritary_column]], decreasing = TRUE)
      } else {
        ord_index <- order(occ_thin[[prioritary_column]], decreasing = FALSE)
      }
      occ_thin <- occ_thin[ord_index, ]
    }

    # Initialize the flag column for the data being thinned ('occ_thin')
    occ_thin$thin_geo_flag <- TRUE

    # --- 2. Conversion and Distance Calculation (Optimized) ---

    # Calculate the distance matrix (in kilometers)
    m <- as.matrix(occ_thin[, c(long, lat)])
    if(!optimize_memory){
      matriz_dist_m <- fields::rdist.earth(x1 = m, miles = FALSE)
    } else {
      matriz_dist_m <- fields::RdistEarth(x1 = m, miles = FALSE)
    }

    # --- 3. Redundant Record Removal ---

    n <- nrow(occ_thin)

    # The loop iterates through the prioritized data
    for (i in 1:n) {
      # If the current record was already flagged for removal, skip.
      if (!occ_thin$thin_geo_flag[i]) {
        next
      }

      # 1. Find all remaining active points that come AFTER the current point 'i'
      indices_posteriores <- which(occ_thin$thin_geo_flag & (1:n) > i)

      if (length(indices_posteriores) == 0) {
        next # No more records to check
      }

      # 2. Check which subsequent records are WITHIN the 'd' radius
      proximos_indices <- indices_posteriores[matriz_dist_m[i, indices_posteriores] < d]

      # 3. Flag close records (which have lower priority) for removal.
      if (length(proximos_indices) > 0) {
        occ_thin$thin_geo_flag[proximos_indices] <- FALSE
      }
    }

    # --- 4. Finalization and Order Restoration ---

    # Select only the original ID and the calculated flag from the thinned data
    thin_geo_flags <- occ_thin[, c("original_id", "thin_geo_flag")]

    # Merge the flag back to the original dataframe using the 'original_id'
    occ_final <- merge(occ_x, thin_geo_flags, by = "original_id", all.x = TRUE)
    occ_final$thin_geo_flag[is.na(occ_final$thin_geo_flag)] <- FALSE
    occ_final$original_id <- NULL

    return(occ_final)
  })
  return(as.data.frame(data.table::rbindlist(res)))
}
