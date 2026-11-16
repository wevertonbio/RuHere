#' Identify records outside natural ranges according to Fauna do Brasil
#'
#' @description
#' Flags occurrence records based on known distribution data
#' from the Catálogo Taxonômico da Fauna do Brasil (`faunabr`) data. This
#' function checks if an occurrence point for a given species falls within its
#' documented distribution, allowing for user-defined buffers around Brazilian
#' states, or the entire country. Records are flagged as `TRUE` if they fall
#' within the specified range for the distribution information available in the
#' `faunabr` data.
#'
#' @param data_dir (character) **Required** directory path where the `faunabr`
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
#' are flagged as `TRUE`. Default is 20 km.
#' @param by_country (logical) if `TRUE`, flags records based on their distance
#' to country distributions. Default is `TRUE`.
#' @param buffer_country (numeric) buffer distance (in kilometers) to be applied
#' around the country boundaries. Records within this distance are flagged as
#' `TRUE`. Default is 20 km.
#' @param keep_columns (logical) if `TRUE`, the returned data frame contains
#' all original columns from `occ`. If `FALSE`, it returns only the key columns
#' and the flag. Default is `TRUE`.
#' @param spat_state (SpatVector) a SpatVector of the Brazilian states. By
#' default, it uses the SpatVector provided by `geobr::read_state()`. It can be
#' another SpatVector, but the structure must be identical to `faunabr::states`,
#' with a column called `"abbrev_state"` identifying the state codes.
#' @param spat_country (SpatVector) a SpatVector of the world countries. By
#' default, it uses the SpatVector provided by `rnaturalearth::ne_countries()`.
#' It can be another SpatVector, but the structure must be identical to
#' `faunabr::world_fauna`, with a column called `"country_code"` identifying the
#' country codes.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If `TRUE`, the `'pbapply'` package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `FALSE`.
#'
#' @details
#' For some taxonomic groups in the Catálogo Taxonômico da Fauna do Brasil
#' (e.g., amphibians and several invertebrate groups), a substantial proportion
#' of valid species entries lack documented state (`states`) or country
#' (`countryCode`) distribution records. When a species is present in the
#' catalogue but lacks distribution information for the enabled spatial filters
#' (`by_state` and/or `by_country`), its records cannot be validated and are
#' assigned `NA` in `faunabr_flag` (consistent with species absent from the
#' catalogue).
#'
#' @return
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{faunabr_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{faunabr}
#' data. As with all other flagging functions in RuHere, \code{TRUE}
#' indicates that the record passed this test and is eligible for retention;
#' \code{FALSE} indicates it failed and is flagged as potentially problematic,
#' and can be removed using \code{remove_flagged()}. Records for species not
#' found in the \code{faunabr} data, or lacking distribution information in the
#' catalogue, will have \code{NA} in the \code{faunabr_flag} column.
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

  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  is_single_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  if (missing(data_dir) || is.null(data_dir) || !is_single_str(data_dir)) {
    stop("'data_dir' must be a single non-empty character string.", call. = FALSE)
  }

  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).", call. = FALSE)
  }
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' should be a data.frame or data.table, not ",
         paste(class(occ), collapse = "/"), ".", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # Force occ to be a data.frame
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  if (!is_single_str(species)) {
    stop("'species' must be a single character string.", call. = FALSE)
  }
  if (!is_single_str(long)) {
    stop("'long' must be a single character string.", call. = FALSE)
  }
  if (!is_single_str(lat)) {
    stop("'lat' must be a single character string.", call. = FALSE)
  }

  core_cols <- c(species, long, lat)
  if (!all(core_cols %in% names(occ))) {
    missing_cols <- setdiff(core_cols, names(occ))
    stop("The following columns specified by 'species', 'long', or 'lat' are missing from 'occ': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  if (!is.null(origin)) {
    if (!is.character(origin) || length(origin) == 0L || any(is.na(origin))) {
      stop("'origin' must be a character vector without NAs or NULL.", call. = FALSE)
    }
    if (!all(tolower(origin) %in% c("native", "cryptogenic", "exotic"))) {
      stop("'origin' contains invalid values. Must be one or more of: 'native', 'cryptogenic', 'exotic'.",
           call. = FALSE)
    }
  }

  if (!is_single_bool(by_state)) {
    stop("'by_state' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(by_country)) {
    stop("'by_country' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!by_state && !by_country) {
    stop("At least one of 'by_state' or 'by_country' must be TRUE.", call. = FALSE)
  }

  if (!is.numeric(buffer_state) || length(buffer_state) != 1L || is.na(buffer_state) || buffer_state < 0) {
    stop("'buffer_state' must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(buffer_country) || length(buffer_country) != 1L || is.na(buffer_country) || buffer_country < 0) {
    stop("'buffer_country' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is_single_bool(keep_columns)) {
    stop("'keep_columns' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  if (!is.null(spat_state) && !inherits(spat_state, "SpatVector")) {
    stop("'spat_state' must be an object of class 'SpatVector' or NULL, not ",
         paste(class(spat_state), collapse = "/"), ".", call. = FALSE)
  }
  if (!is.null(spat_country) && !inherits(spat_country, "SpatVector")) {
    stop("'spat_country' must be an object of class 'SpatVector' or NULL, not ",
         paste(class(spat_country), collapse = "/"), ".", call. = FALSE)
  }

  if (!file.exists(file.path(data_dir, "faunabr/"))) {
    stop("Data from faunabr necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'faunabr_here()' function.", call. = FALSE)
  }

  if (!is_single_bool(progress_bar)) {
    stop("'progress_bar' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (progress_bar) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      my_lapply <- pbapply::pblapply
    } else {
      stop("Package 'pbapply' is required if 'progress_bar = TRUE'. Run install.packages('pbapply')",
           call. = FALSE)
    }
  } else {
    my_lapply <- base::lapply
  }

  if (!is_single_bool(verbose)) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # ---- LOAD & FILTER CATALOGUE ----
  d <- faunabr::load_faunabr(file.path(data_dir, "faunabr/"),
                             type = "complete", verbose = FALSE)

  unique_occ_spp <- unique(occ[[species]])
  spp_in <- intersect(unique_occ_spp, unique(d$species))
  spp_out <- setdiff(unique_occ_spp, spp_in)

  if (length(spp_out) > 0L && verbose) {
    warning("Some species present in 'occ' will not be checked due to absence in faunabr: ",
            paste(spp_out, collapse = ", "), call. = FALSE)
  }

  if (length(spp_in) == 0L) {
    stop("None of the species in 'occ' was found in the available faunabr data.",
         call. = FALSE)
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique_occ_spp), " species")
  }

  # Subset and pre-split both catalogue and occurrence data by species (avoids O(N*S) scans)
  d_sub <- d[d$species %in% spp_in, , drop = FALSE]
  if (!is.null(origin)) {
    d_sub$origin <- tolower(d_sub$origin)
  }
  d_split <- split(d_sub, d_sub$species)

  occ_in <- occ[occ[[species]] %in% spp_in, , drop = FALSE]
  occ_split <- split(occ_in, occ_in[[species]])

  origin_target <- if (!is.null(origin)) tolower(origin) else NULL
  no_dist_spp <- character(0L)

  # Helper to check if a vector contains any non-empty, non-NA string
  has_info <- function(x) {
    if (is.null(x) || length(x) == 0L) return(FALSE)
    any(!is.na(x) & nzchar(trimws(as.character(x))))
  }

  res_flag <- my_lapply(spp_in, function(i) {
    d_i <- d_split[[i]]
    occ_spec_i <- occ_split[[i]]

    if (!is.null(origin_target)) {
      d_i <- d_i[d_i$origin %in% origin_target, , drop = FALSE]
    }

    # Remove columns if they already exist
    to_remove <- intersect(c("states", "countrycode", "inside_country", "inside_state"),
                           colnames(occ_spec_i))
    if (length(to_remove) > 0L) {
      occ_spec_i[, to_remove] <- NULL
    }

    # If filtered by origin and no matching rows remain
    if (nrow(d_i) == 0L) {
      occ_spec_i$faunabr_flag <- FALSE
      return(occ_spec_i)
    }

    # Check if catalogue has distribution data for the enabled filters
    has_state_data <- has_info(d_i$states)
    has_country_data <- has_info(d_i$countryCode)

    missing_dist <- (by_state && by_country && !has_state_data && !has_country_data) ||
      (by_state && !by_country && !has_state_data) ||
      (!by_state && by_country && !has_country_data)

    if (missing_dist) {
      no_dist_spp <<- c(no_dist_spp, i)
      if (!keep_columns) {
        occ_spec_i <- occ_spec_i[, core_cols, drop = FALSE]
      }
      occ_spec_i$faunabr_flag <- NA
      return(occ_spec_i)
    }

    occ_i <- faunabr::filter_faunabr(data = d_i,
                                     occ = occ_spec_i,
                                     species = species,
                                     long = long,
                                     lat = lat,
                                     value = "flag",
                                     by_state = by_state && has_state_data,
                                     buffer_state = buffer_state,
                                     by_country = by_country && has_country_data,
                                     buffer_country = buffer_country,
                                     keep_columns = keep_columns,
                                     spat_state = spat_state,
                                     spat_country = spat_country,
                                     verbose = FALSE)

    colnames(occ_i)[colnames(occ_i) == "filters_ok"] <- "faunabr_flag"
    return(occ_i)
  })

  if (length(no_dist_spp) > 0L && verbose) {
    message("No distribution data in faunabr for ", length(no_dist_spp),
            " species (flagged as NA): ", paste(no_dist_spp, collapse = ", "))
  }

  # Combine per-species results safely with fill = TRUE
  res_flag <- as.data.frame(data.table::rbindlist(res_flag, fill = TRUE))

  # Append species absent from catalogue with faunabr_flag = NA
  if (length(spp_out) > 0L) {
    occ_out <- occ[occ[[species]] %in% spp_out, , drop = FALSE]

    if (nrow(occ_out) > 0L) {
      if (!keep_columns) {
        occ_out <- occ_out[, core_cols, drop = FALSE]
      }
      occ_out$faunabr_flag <- NA
      res_flag <- as.data.frame(data.table::rbindlist(list(res_flag, occ_out), fill = TRUE))
    }
  }

  return(res_flag)
}
