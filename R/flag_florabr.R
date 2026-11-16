#' Identify records outside natural ranges according to Flora e Funga do Brasil
#'
#' @description
#' Flags occurrence records based on known distribution data
#' from the Flora e Funga do Brasil (`florabr`) data. This function checks if an
#' occurrence point for a given species falls within its documented distribution,
#' allowing for user-defined buffers around Brazilian states, biomes, or the
#' entire country. Records are flagged as `TRUE` if they fall within
#' the specified range for the distribution information available in the
#' `florabr` data.
#'
#' @param data_dir (character) directory path where the `florabr` data is
#' saved. **Required.**
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be flagged. Must contain columns for species, longitude, and
#' latitude.
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
#' are flagged as `TRUE`. Default is 20 km.
#' @param by_biome (logical) if `TRUE`, flags records based on their
#' distance to known Brazilian biome distributions. Default is `TRUE`.
#' @param buffer_biome (numeric) buffer distance (in kilometers) to be
#' applied around the known biome distribution boundaries. Records within this
#' distance are flagged as `TRUE`. Default is 20 km.
#' @param by_endemism (logical) if `TRUE`, includes a check against the entire
#' Brazilian boundary. Default is `TRUE`.
#' @param buffer_brazil (numeric) buffer distance (in kilometers) to be applied
#' around the entire Brazilian boundary. Default is 20 km.
#' @param state_vect (SpatVector) an optional custom spatial vector
#' (`SpatVector`) representing Brazilian states/regions. If `NULL`, uses the
#' default data loaded by `florabr`. Default is `NULL`.
#' @param state_column (character) the name of the column in `state_vect`
#' (or the default state vector) used to match distribution information.
#' Default is `NULL`.
#' @param biome_vect (SpatVector) an optional custom spatial vector
#' (`SpatVector`) representing Brazilian biomes. If `NULL`, uses the default data
#' loaded by `florabr`. Default is `NULL`.
#' @param biome_column (character) the name of the column in `biome_vect`
#' (or the default biome vector) used to match distribution information.
#' Default is `NULL`.
#' @param br_vect (SpatVector) an optional custom spatial vector
#' (`SpatVector`) representing the entire Brazilian boundary. If `NULL`, uses
#' the default data loaded by `florabr`. Default is `NULL`.
#' @param keep_columns (logical) if `TRUE`, the returned data frame contains
#' all original columns from `occ`. If `FALSE`, it returns only the key columns
#' and the flag. Default is `TRUE`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If `TRUE`, the `'pbapply'` package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `FALSE`.
#'
#' @details
#' For some taxonomic groups in the Flora e Funga do Brasil (e.g., algae,
#' fungi, and certain bryophyte or angiosperm groups), a substantial proportion
#' of valid species entries lack documented state (`states`), biome (`biome`),
#' or endemism (`endemism`) distribution records. When a species is present in
#' the catalogue but lacks distribution information for the enabled spatial
#' filters (`by_state`, `by_biome`, and/or `by_endemism`), its records cannot be
#' validated and are assigned `NA` in `florabr_flag` (consistent with species
#' absent from the catalogue).
#'
#' @return
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{florabr_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{florabr}
#' data. As with all other flagging functions in RuHere, \code{TRUE}
#' indicates that the record passed this test and is eligible for retention;
#' \code{FALSE} indicates it failed and is flagged as potentially problematic,
#' and can be removed using \code{remove_flagged()}. Records for species not
#' found in the \code{florabr} data, or lacking distribution information in the
#' catalogue, will have \code{NA} in the \code{florabr_flag} column.
#'
#' @importFrom florabr load_florabr filter_florabr
#' @importFrom data.table rbindlist
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
    valid_origins <- c("native", "cultivated", "naturalized", "unknown", "not_found_in_brazil")
    if (!all(tolower(origin) %in% valid_origins)) {
      stop("'origin' contains invalid values. Must be one or more of: 'native', 'cultivated', 'naturalized', 'unknown', 'not_found_in_brazil'.",
           call. = FALSE)
    }
  }

  if (!is_single_bool(by_state)) {
    stop("'by_state' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(by_biome)) {
    stop("'by_biome' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!is_single_bool(by_endemism)) {
    stop("'by_endemism' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }
  if (!by_state && !by_biome && !by_endemism) {
    stop("At least one of 'by_state', 'by_biome', or 'by_endemism' must be TRUE.",
         call. = FALSE)
  }

  if (!is.numeric(buffer_state) || length(buffer_state) != 1L || is.na(buffer_state) || buffer_state < 0) {
    stop("'buffer_state' must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(buffer_biome) || length(buffer_biome) != 1L || is.na(buffer_biome) || buffer_biome < 0) {
    stop("'buffer_biome' must be a single non-negative numeric value.", call. = FALSE)
  }
  if (!is.numeric(buffer_brazil) || length(buffer_brazil) != 1L || is.na(buffer_brazil) || buffer_brazil < 0) {
    stop("'buffer_brazil' must be a single non-negative numeric value.", call. = FALSE)
  }

  if (!is.null(state_vect) && !inherits(state_vect, "SpatVector")) {
    stop("'state_vect' must be an object of class 'SpatVector' or NULL, not ",
         paste(class(state_vect), collapse = "/"), ".", call. = FALSE)
  }
  if (!is.null(state_column) && !is_single_str(state_column)) {
    stop("'state_column' must be a single character string or NULL.", call. = FALSE)
  }

  if (!is.null(biome_vect) && !inherits(biome_vect, "SpatVector")) {
    stop("'biome_vect' must be an object of class 'SpatVector' or NULL, not ",
         paste(class(biome_vect), collapse = "/"), ".", call. = FALSE)
  }
  if (!is.null(biome_column) && !is_single_str(biome_column)) {
    stop("'biome_column' must be a single character string or NULL.", call. = FALSE)
  }

  if (!is.null(br_vect) && !inherits(br_vect, "SpatVector")) {
    stop("'br_vect' must be an object of class 'SpatVector' or NULL, not ",
         paste(class(br_vect), collapse = "/"), ".", call. = FALSE)
  }

  if (!is_single_bool(keep_columns)) {
    stop("'keep_columns' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  if (!file.exists(file.path(data_dir, "florabr/"))) {
    stop("Data from florabr necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'florabr_here()' function.", call. = FALSE)
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
  d <- florabr::load_florabr(file.path(data_dir, "florabr/"), type = "complete",
                             verbose = FALSE)

  unique_occ_spp <- unique(occ[[species]])
  spp_in <- intersect(unique_occ_spp, unique(d$species))
  spp_out <- setdiff(unique_occ_spp, spp_in)

  if (length(spp_out) > 0L && verbose) {
    warning("Some species present in 'occ' will not be checked due to absence in florabr: ",
            paste(spp_out, collapse = ", "), call. = FALSE)
  }

  if (length(spp_in) == 0L) {
    stop("None of the species in 'occ' was found in the available florabr data.",
         call. = FALSE)
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique_occ_spp), " species")
  }

  # Subset and pre-split both catalogue and occurrence data by species
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

    # Remove residual columns if they already exist
    to_remove <- intersect(c("states", "biome", "endemism",
                             "inside_br", "inside_state", "inside_biome"),
                           colnames(occ_spec_i))
    if (length(to_remove) > 0L) {
      occ_spec_i[, to_remove] <- NULL
    }

    if (nrow(d_i) == 0L) {
      if (!keep_columns) {
        occ_spec_i <- occ_spec_i[, core_cols, drop = FALSE]
      }
      occ_spec_i$florabr_flag <- FALSE
      return(occ_spec_i)
    }

    # Check whether catalogue has distribution data for the enabled spatial filters
    has_state_data <- has_info(d_i$states)
    has_biome_data <- has_info(d_i$biome)
    has_endemism_data <- has_info(d_i$endemism)

    any_enabled_has_data <- (by_state && has_state_data) ||
      (by_biome && has_biome_data) ||
      (by_endemism && has_endemism_data)

    if (!any_enabled_has_data) {
      no_dist_spp <<- c(no_dist_spp, i)
      if (!keep_columns) {
        occ_spec_i <- occ_spec_i[, core_cols, drop = FALSE]
      }
      occ_spec_i$florabr_flag <- NA
      return(occ_spec_i)
    }

    occ_i <- florabr::filter_florabr(data = d_i,
                                     occ = occ_spec_i,
                                     species = species,
                                     long = long,
                                     lat = lat,
                                     value = "flag",
                                     by_state = by_state && has_state_data,
                                     buffer_state = buffer_state,
                                     by_biome = by_biome && has_biome_data,
                                     buffer_biome = buffer_biome,
                                     by_endemism = by_endemism && has_endemism_data,
                                     buffer_brazil = buffer_brazil,
                                     state_vect = state_vect,
                                     state_column = state_column,
                                     biome_vect = biome_vect,
                                     biome_column = biome_column,
                                     br_vect = br_vect,
                                     keep_columns = keep_columns,
                                     verbose = FALSE)

    colnames(occ_i)[colnames(occ_i) == "filters_ok"] <- "florabr_flag"
    return(occ_i)
  })

  if (length(no_dist_spp) > 0L && verbose) {
    message("No distribution data in florabr for ", length(no_dist_spp),
            " species (flagged as NA): ", paste(no_dist_spp, collapse = ", "))
  }

  # Combine per-species results safely with fill = TRUE
  res_flag <- as.data.frame(data.table::rbindlist(res_flag, fill = TRUE))

  # Append species absent from catalogue with florabr_flag = NA
  if (length(spp_out) > 0L) {
    occ_out <- occ[occ[[species]] %in% spp_out, , drop = FALSE]

    if (nrow(occ_out) > 0L) {
      if (!keep_columns) {
        occ_out <- occ_out[, core_cols, drop = FALSE]
      }
      occ_out$florabr_flag <- NA
      res_flag <- as.data.frame(data.table::rbindlist(list(res_flag, occ_out), fill = TRUE))
    }
  }

  return(res_flag)
}
