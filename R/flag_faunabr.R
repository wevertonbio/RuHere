#' Identify records outside natural ranges according to Fauna do Brasil
#'
#' @description
#' Flags (validates) occurrence records based on known distribution data
#' from the Catálogo Taxônomico da Fauna do Brasil (faunabr) data. This function
#' checks if an occurrence point for a given species falls within its documented
#' distribution, allowing for user-defined buffers around Brazilian states,
#' or the entire country. Records are flagged as valid (`TRUE`) if they fall
#' within the specified range for the distribution information available in the
#' `faunabr` data.
#'
#' @param data_dir (character)  **Required** directory path where the `faunabr`
#' data is saved.
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be flagged. Must contain columns for species, longitude, and
#' latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param origin (character) filter the `faunabr` data by origin type
#' before checking (`"native"`, `"cryptogenic"`, or `"exotic"`). Default is
#' `NULL` (no filtering).
#' @param by_state (logical) if `TRUE`, flags records based on their distance
#' to known Brazilian state distributions. Default is `TRUE`.
#' @param buffer_state (numeric) buffer distance (in kilometers) to be applied
#' around the known state distribution boundaries. Records within this distance
#' are considered valid. Default is 20 km.
#' @param by_country (logical) if `TRUE`, flags records based on their distance
#' to country distributions. Default is `TRUE`.
#' @param buffer_country (numeric) buffer distance (in kilometers) to be applied
#' around the country boundaries. Records within this distance are considered
#' valid. Default is 20 km.
#' @param keep_columns (logical) if `TRUE`, the returned data frame contains
#' all original columns from `occ`. If `FALSE`, it returns only the key columns
#' and the flag. Default is `TRUE`.
#' @param spat_state (SpatVector) a SpatVector of the Brazilian states. By
#' default, it uses the SpatVector provided by geobr::read_state(). It can be
#' another Spatvector, but the structure must be identical to 'faunabr::states',
#' with a column called "abbrev_state" identifying the states codes.
#' @param spat_country (SpatVector) a SpatVector of the world countries. By
#' default, it uses the SpatVector provided by rnaturalearth::ne_countries. It
#' can be another Spatvector, but the structure must be identical to
#' 'faunabr::world_fauna', with a column called "country_code" identifying the
#' country codes.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `FALSE`.
#'
#' @return #' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{faunabr_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{faunabr}
#' data. Records for species not found in the \code{faunabr} data will have
#' \code{NA} in the \code{faunabr_flag} column.
#'
#' @importFrom faunabr load_faunabr filter_faunabr
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Get only occurrences from Azure Jay
#' occ <- occurrences[occurrences$species == "Cyanocorax caeruleus", ]
#' # Set folder where distributional datasets were saved
#' # Here, just a sample provided in the package
#' # You must run 'faunabr_here()' beforehand to download the necessary data files for your species
#' dataset_dir <- system.file("extdata/datasets", package = "RuHere")
#' # Flag records using faunabr specialist information
#' occ_fauna <- flag_faunabr(data_dir = dataset_dir, occ = occ)
flag_faunabr <- function(data_dir, occ, species = "species",
                         long = "decimalLongitude", lat = "decimalLatitude",
                         origin = NULL, by_state = TRUE,
                         buffer_state = 20, by_country = TRUE,
                         buffer_country = 20, keep_columns = TRUE,
                         spat_state = NULL, spat_country = NULL,
                         progress_bar = FALSE, verbose = FALSE) {


  if (missing(data_dir) || is.null(data_dir)) {
    stop("'data_dir' is required (must not be NULL or missing).")
  }
  if (!inherits(data_dir, "character")) {
    stop("'data_dir' must be a character, not ", class(data_dir))
  }

  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).")
  } else if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' should be a data.frame or data.table, not ", class(occ))
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)


  if (!inherits(species, "character")) {
    stop("'species' should be a character, not ", class(species))
  }

  if (!inherits(long, "character")) {
    stop("'long' should be a character, not ", class(long))
  }

  if (!inherits(lat, "character")) {
    stop("'lat' should be a character, not ", class(lat))
  }


  if (!is.null(origin)) {
    if (!inherits(origin, "character")) {
      stop("'origin' must be a character vector or NULL, not ", class(origin))
    }
    if (!all(tolower(origin) %in% c("native", "cryptogenic", "exotic"))) {
      stop("'origin' contains invalid values. Must be one or more of: 'native',
           'cryptogenic', 'exotic'")
    }
  }

  if (!inherits(by_state, "logical")) {
    stop("'by_state' must be logical, not ", class(by_state))
  }

  if (!inherits(buffer_state, "numeric")) {
    stop("'buffer_state' must be numeric, not ", class(buffer_state))
  }

  if (!inherits(by_country, "logical")) {
    stop("'by_country' must be logical, not ", class(by_country))
  }

  if (!inherits(buffer_country, "numeric")) {
    stop("'buffer_country' must be numeric, not ", class(buffer_country))
  }

  if (!inherits(keep_columns, "logical")) {
    stop("'keep_columns' must be logical, not ", class(keep_columns))
  }

  if (!is.null(spat_state)) {
    if (!inherits(spat_state, "SpatVector")) {
      stop("'spat_state' must be an object of class 'SpatVector' or NULL, not ", class(spat_state), call. = FALSE)
    }
  }

  if (!is.null(spat_country)) {
    if (!inherits(spat_country, "SpatVector")) {
      stop("'spat_country' must be an object of class 'SpatVector' or NULL, not ", class(spat_country), call. = FALSE)
    }
  }

  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "faunabr/"))){
    stop("Data from faunabr necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'faunabr_here()' function")
  }

  core_cols <- c(species, long, lat)
  if (!all(core_cols %in% names(occ))) {
    missing_cols <- setdiff(core_cols, names(occ))
    stop("The following columns specified by 'species', 'long', or 'lat' are missing from 'occ': ", paste(missing_cols, collapse = ", "))
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

  # verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  # Import data
  d <- faunabr::load_faunabr(file.path(data_dir, "faunabr/"),
                             type = "complete", verbose = FALSE)

  # Get species in data
  spp_in <- intersect(unique(occ[[species]]), unique(d$species))
  spp_out <- setdiff(unique(occ[[species]]), unique(d$species))

  #Warning if some species are not available
  if (length(spp_out) > 0 && verbose) {
    warning("Some species present in occ will not be checked due to absence of information in faunabr")
  }

  if (length(spp_in) == 0) {
    stop("None of the species in occ has information in the faunabr data available")
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique(occ[["species"]])), " species")
  }

  occ_in <- occ[occ[[species]] %in% spp_in, ]

  origin_target <- if(!is.null(origin)) tolower(origin) else NULL

  res_flag <- my_lapply(spp_in, function(i) {

    d_i <- d[d$species == i, ]

    if(!is.null(origin_target)) {
      d_i$origin <- tolower(d_i$origin)
      d_i <- d_i[d_i$origin %in% origin_target, ]
    }

    occ_spec_i <- occ_in[occ_in[[species]] == i, ]

    # Remove columns, if it alrady exists
    to_remove <- intersect(c("states", "inside_br", "inside_state"),
                           colnames(occ_spec_i))
    if(length(to_remove) > 0){
      occ_spec_i[, to_remove] <- NULL
    }


    if(nrow(d_i) == 0) {
      occ_spec_i$faunabr_flag <- FALSE
      return(occ_spec_i)
    }

    occ_i <- faunabr::filter_faunabr(data = d_i, occ = occ_spec_i,
                                     species = species,
                                     long = long,
                                     lat = lat,
                                     value = "flag",
                                     by_state = by_state,
                                     buffer_state = buffer_state,
                                     by_country = by_country,
                                     buffer_country = buffer_country,
                                     keep_columns = keep_columns,
                                     spat_state = spat_state,
                                     spat_country = spat_country,
                                     verbose = verbose)

    colnames(occ_i)[colnames(occ_i) == "filters_ok"] <- "faunabr_flag"

    return(occ_i)

  })

  res_flag <- as.data.frame((data.table::rbindlist(res_flag)))

  if (length(spp_out) > 0) {
    occ_out <- occ[occ[[species]] %in% spp_out, ]

    if(nrow(occ_out) > 0) {
      occ_out$faunabr_flag <- NA
      if (!keep_columns) {
        common_cols <- intersect(colnames(occ_out), colnames(res_flag))
        occ_out <- occ_out[, common_cols, drop = FALSE]
        res_flag <- res_flag[, common_cols, drop = FALSE]
      }

      res_flag <- as.data.frame(data.table::rbindlist(list(res_flag, occ_out), fill = TRUE))
      }
  }

  return(res_flag)

}
