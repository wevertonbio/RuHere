#' Identify Environmental Blocks and Group Nearby Records in Environmental Space
#'
#' @description
#' This function creates a multidimensional grid in environmental space by
#' splitting each environmental variable into `n_bins` equally sized intervals.
#' It then assigns each occurrence record to an environmental block (bin
#' combination) and identifies records that fall into the same block (i.e.,
#' records that are close to each other in environmental space).
#'
#' The results can be visualized using the `plot_env_bins()` function.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records for a **single species**. Must contain columns for species, longitude, and latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param env_layers (SpatRaster) object containing environmental variables.
#' @param n_bins (numeric) number of bins into which each environmental variable
#' will be divided.
#'
#' @return
#' A list with:
#'   - **data**: a data frame including extracted environmental values, bin
#'     indices, and a unique `block_id` for each record.
#'   - **breaks**: a named list of numeric vectors containing the break points
#'     for each variable (used by `plot_env_bins()`).
#'
#' @importFrom terra extract
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Get only occurrences from Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Import environmental layers
#' r <- terra::rast(system.file("extdata", "worldclim.tif",
#'                              package = "RuHere"))
#' # Get bins
#' b <- get_env_bins(occ = occ, env_layers = r, n_bins = 5)
#'
get_env_bins <- function(occ, species = "species",
                         long = "decimalLongitude",
                         lat = "decimalLatitude",
                         env_layers, n_bins = 5) {

  #========================#
  #  ARGUMENT CHECKING     #
  #========================#

  # occ
  if (missing(occ) || is.null(occ)) {
    stop("'occ' must be provided (cannot be NULL or missing).")
  } else if (!inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame, not ", class(occ))
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # species column
  if (!is.character(species) || length(species) != 1) {
    stop("'species' must be a single character string")
  }
  if (!species %in% names(occ)) {
    stop(paste0("Column '", species, "' not found in 'occ'"))
  }

  # Check only one species
  n_spp <- length(unique(occ[[species]]))
  if (n_spp > 1) {
    stop("'occ' must contain occurrences of a single species")
  }

  # longitude column
  if (!is.character(long) || length(long) != 1) {
    stop("'long' must be a single character string")
  }
  if (!long %in% names(occ)) {
    stop(paste0("Column '", long, "' not found in 'occ'"))
  }

  # latitude column
  if (!is.character(lat) || length(lat) != 1) {
    stop("'lat' must be a single character string")
  }
  if (!lat %in% names(occ)) {
    stop(paste0("Column '", lat, "' not found in 'occ'"))
  }

  # env_layers
  if (!inherits(env_layers, "SpatRaster")) {
    stop("'env_layers' must be a SpatRaster object")
  }
  if (terra::nlyr(env_layers) < 1) {
    stop("'env_layers' must contain at least one layer")
  }

  # n_bins
  if (!is.numeric(n_bins) || length(n_bins) != 1) {
    stop("'n_bins' must be a single numeric value")
  }
  if (n_bins < 2) {
    stop("'n_bins' must be >= 2 to define environmental bins")
  }


  # 1. Extract environmental values at point locations
  # ID = FALSE prevents terra from adding a row ID column
  env_values <- terra::extract(env_layers, occ[, c(long, lat)], ID = FALSE)

  # # Check for NAs (points outside raster extent)
  # na_values <- apply(env_values, 1, anyNA)

  # 2. Calculate breaks and assign bins for each variable
  # We use a loop to handle each column specifically and store the break values

  bin_indices <- list() # To store the integer index (1, 2, 3...)
  break_values <- list() # To store the actual numeric cut points

  for (var_name in names(env_values)) {
    x <- env_values[[var_name]]

    # Define breaks covering the range of the data
    # We add a tiny epsilon to the max to ensure the last point is included
    rng <- range(x, na.rm = TRUE)
    breaks <- seq(rng[1], rng[2], length.out = n_bins + 1)

    # Store breaks for plotting later
    break_values[[var_name]] <- breaks

    # Cut the data to get the bin index (integer)
    bin_indices[[paste0(var_name, "_bin")]] <- as.integer(
      cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
    )
  }

  # Convert list of indices to data frame
  df_bins <- as.data.frame(bin_indices)

  # 3. Create the unique Block ID
  # Combines all bin indices into a string (e.g., "3_5_1")
  df_bins$block_id <- apply(df_bins, 1, function(row) {
    if (any(is.na(row))) {
      return(NA_character_) # Return NA if any bin is NA
    } else {
      return(paste(row, collapse = "_")) # Otherwise, paste the bins
    }
  })

  # 4. Prepare final data frame
  # Combine extracted environmental values + bin indices + block_id
  final_data <- cbind(env_values, df_bins)

  # Return list with data and the breaks definitions
  return(list(
    data = final_data,
    breaks = break_values
  ))
}
