#' Identify records outside natural ranges according to Flora e Funga do Brasil
#'
#' @description
#' Flags (validates) occurrence records based on known distribution data
#' from the Flora e Funga do Brasil (florabr) data. This function checks if an
#' occurrence point for a given species falls within its documented distribution,
#' allowing for user-defined buffers around Brazilian states, biomes, or the
#' entire country. Records are flagged as valid (`TRUE`) if they fall within
#' the specified range for the distribution information available in the
#' `florabr` data.
#'
#' @param data_dir (character) directory path where the `florabr` data is
#' saved **Required.**
#' @param occ (data.frame) a data frame containing the occurrence records to be
#' flagged. Must contain columns for species, longitude, and latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param origin (character or NULL) filter the `florabr` data by origin type
#' before checking (`"native"`, `"cultivated"`, `"naturalized"`, `"unknown"`,
#' or `"not_found_in_brazil"`). Default is `NULL` (no filtering).
#' @param by_state (logical) if `TRUE`, flags records based on their distance
#' to known Brazilian state distributions. Default is `TRUE`.
#' @param buffer_state (numeric) buffer distance (in kilometers) to be applied
#' around the known state distribution boundaries. Records within this distance
#' are considered valid. Default is 20 km.
#' @param by_biome (logical) if `TRUE`, flags records based on their
#' distance to known Brazilian biome distributions. Default is `TRUE`.
#' @param buffer_biome (numeric) buffer distance (in kilometers) to be
#' applied around the known biome distribution boundaries. Records within this
#' distance are considered valid. Default is 20 km.
#' @param by_endemism (logical) if `TRUE`, includes a check against the entire
#' Brazilian boundary. Default is `TRUE`.
#' @param buffer_brazil (numeric) buffer distance (in kilometers) to be applied
#' around the entire Brazilian boundary. Default is 20 km.
#' @param state_vect (SpatVector) qn optional custom simple features
#' (`sf`) vector representing Brazilian states/regions. If `NULL`, uses the
#' default data loaded by `florabr`. Default is `NULL`.
#' @param state_column (character) the name of the column in `state_vect`
#' (or the default state vector) used to match distribution information.
#' Default is `NULL`.
#' @param biome_vect (SpatVector) an optional custom simple features (`sf`)
#' vector representing Brazilian biomes. If `NULL`, uses the default data
#' loaded by `florabr`. Default is `NULL`.
#' @param biome_column (character) the name of the column in `biome_vect`
#' (or the default biome vector) used to match distribution information.
#' Default is `NULL`.
#' @param br_vect (SpatVector) an optional custom simple features (`sf`) vector
#' representing the entire Brazilian boundary. If `NULL`, uses the default data
#' loaded by `florabr`. Default is `NULL`.
#' @param keep_columns (logical) if `TRUE`, the returned data frame contains
#' all original columns from `occ`. If `FALSE`, it returns only the key columns
#' and the flag. Default is `TRUE`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `FALSE`.
#'
#' @return
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{florabr_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{florabr}
#' data. Records for species not found in the \code{florabr} data will have
#' \code{NA} in the \code{florabr_flag} column.
#'
#' @importFrom florabr load_florabr filter_florabr
#' @importFrom data.table data.table rbindlist
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Get only occurrences from Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Set folder where distributional datasets were saved
#' # Here, just a sample provided in the package
#' # You must run 'florabr_here()' beforehand to download the necessary data files for your species
#' dataset_dir <- system.file("extdata/datasets", package = "RuHere")
#'
#' # Flag records using specialist information from Flora do Brasil
#' occ_flora <- flag_florabr(data_dir = dataset_dir, occ = occ)
#'
flag_florabr <- function(data_dir, occ, species = "species",
                         long = "decimalLongitude", lat = "decimalLatitude",
                         origin = NULL,
                         by_state = TRUE, buffer_state = 20,
                         by_biome = TRUE, buffer_biome = 20, by_endemism = TRUE,
                         buffer_brazil = 20, state_vect = NULL,
                         state_column = NULL, biome_vect = NULL,
                         biome_column = NULL, br_vect = NULL,
                         keep_columns = TRUE, progress_bar = FALSE,
                         verbose = FALSE) {

  if (missing(data_dir) || is.null(data_dir)) {
    stop("'data_dir' is required (must not be NULL or missing).")
  }
  if (!inherits(data_dir, "character")) {
    stop("'data_dir' must be a character, not ", class(data_dir))
  }

  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).")
  } else if (!inherits(occ, "data.frame")) {
    stop("'occ' should be a data.frame, not ", class(occ))
  }

  if (!inherits(species, "character")) {
    stop("'species' should be a character, not ", class(species))
  }

  if (!inherits(long, "character")) {
    stop("'long' should be a character, not ", class(long))
  }

  if (!inherits(lat, "character")) {
    stop("'lat' should be a character, not ", class(lat))
  }

  if (!inherits(verbose, "logical")) {
    stop("'verbose' must be logical, not ", class(verbose))
  }

  if (!is.null(origin)) {
    if (!inherits(origin, "character")) {
      stop("'origin' must be a character vector or NULL, not ", class(origin))
    }
    if (!all(tolower(origin) %in% c("native", "cultivated", "naturalized", "unknown", "not_found_in_brazil"))) {
      stop("'origin' contains invalid values. Must be one or more of: 'native',
           'cultivated', 'naturalized', 'unknown', 'not_found_in_brazil'")
    }
  }

  if (!inherits(by_state, "logical")) {
    stop("'by_state' must be logical, not ", class(by_state))
  }

  if (!inherits(buffer_state, "numeric")) {
    stop("'buffer_state' must be numeric, not ", class(buffer_state))
  }

  if (!inherits(by_biome, "logical")) {
    stop("'by_biome' must be logical, not ", class(by_biome))
  }

  if (!inherits(buffer_biome, "numeric")) {
    stop("'buffer_biome' must be numeric, not ", class(buffer_biome))
  }

  if (!inherits(by_endemism, "logical")) {
    stop("'by_endemism' must be logical, not ", class(by_endemism))
  }

  if (!inherits(buffer_brazil, "numeric")) {
    stop("'buffer_brazil' must be numeric, not ", class(buffer_brazil))
  }

  if (!is.null(state_vect)) {
    if (!inherits(state_vect, "SpatVector")) {
      stop("'state_vect' must be an object of class 'SpatVector' or NULL, not ", class(state_vect), call. = FALSE)
    }
  }

  if (!is.null(state_column) && !inherits(state_column, "character")) {
    stop("'state_column' must be a character or NULL, not ", class(state_column))
  }

  if (!is.null(biome_vect)) {
    if (!inherits(biome_vect, "SpatVector")) {
      stop("'biome_vect' must be an object of class 'SpatVector' or NULL, not ", class(biome_vect), call. = FALSE)
    }
  }

  if (!is.null(biome_column) && !inherits(biome_column, "character")) {
    stop("'biome_column' must be a character or NULL, not ", class(biome_column))
  }

  if (!is.null(br_vect)) {
    if (!inherits(br_vect, "SpatVector")) {
      stop("'br_vect' must be an object of class 'SpatVector' or NULL, not ", class(br_vect), call. = FALSE)
    }
  }

  if (!inherits(keep_columns, "logical")) {
    stop("'keep_columns' must be logical, not ", class(keep_columns))
  }

  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "florabr/"))){
    stop("Data from florabr necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'florabr_here()' function")
  }

  # progress_bar
  if (!inherits(progress_bar, "logical") || length(progress_bar) != 1) {
    stop("'progress_bar' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  if (progress_bar) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      my_lapply <- pbapply::pblapply
    } else {
      stop("Package 'pbapply' is required if 'progress_bar = TRUE'.
Run install.packages('pbapply')", call. = FALSE)
    }
  } else {
    my_lapply <- base::lapply
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)


  core_cols <- c(species, long, lat)
  if (!all(core_cols %in% names(occ))) {
    missing_cols <- setdiff(core_cols, names(occ))
    stop("The following columns specified by 'species', 'long', or 'lat' are missing from 'occ': ", paste(missing_cols, collapse = ", "))
  }

  # Import data
  d <- florabr::load_florabr(file.path(data_dir, "florabr/"), type = "complete",
                             verbose = verbose)

  # Get species in data
  spp_in <- intersect(unique(occ[[species]]),
                      unique(d$species))
  spp_out <- setdiff(unique(occ[[species]]),
                     unique(d$species))

  #Warning if some species are not available
  if (length(spp_out) > 0) {
    warning("Some species present in occ will not be checked due to absence of information in florabr")
  }

  if (length(spp_in) == 0) {
    stop("None of the species in occ has information in the florabr data available")
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique(occ[["species"]])), " species")
  }

  occ_in <- occ[occ[[species]] %in% spp_in, ]

  res_flag <- my_lapply(spp_in, function(i) {

    d_i <- d[d$species == i, ]

    if(!is.null(origin)) {
      origin <- tolower(origin)
      d_i$origin <- tolower(d_i$origin)
      d_i <- d_i[d_i$origin %in% origin, ]
    }

    occ_i <- florabr::filter_florabr(data = d_i, occ = occ_in,
                                     species = species,
                                     long = long,
                                     lat = lat,
                                     value = "flag",
                                     by_state = by_state,
                                     buffer_state = buffer_state,
                                     by_biome = by_biome,
                                     buffer_biome = buffer_biome,
                                     by_endemism = by_endemism,
                                     buffer_brazil = buffer_brazil,
                                     state_vect = state_vect,
                                     state_column = state_column,
                                     biome_vect = biome_vect,
                                     biome_column = biome_column,
                                     br_vect = br_vect,
                                     keep_columns = keep_columns)

    colnames(occ_i)[colnames(occ_i) == "filters_ok"] <- "florabr_flag"

    return(occ_i)

  })

  res_flag <- as.data.frame((data.table::rbindlist(res_flag)))

  if (length(spp_out) > 0) {
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$florabr_flag <- NA
    res_flag <- rbind(res_flag, occ_out)
  }

  return(res_flag)

}
