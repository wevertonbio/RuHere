#' Check if the records fall in the state assigned in the metadata
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records,
#' preferably with state information standardized using
#' `standardize_states()`.
#' @param long (character) column name with longitude. Default is
#' `'decimalLongitude'`.
#' @param lat (character) column name with latitude. Default is
#' `'decimalLatitude'`.
#' @param state_column (character) column name containing the state information.
#' @param distance (numeric) maximum distance (in kilometers) a record can fall
#' outside the state assigned in the `state_column`. Default is `5`.
#' @param try_to_fix (logical) whether to check if coordinates are inverted or
#' transposed (see `fix_states()` for details). If `TRUE`, coordinates
#' identified as inverted or transposed will be corrected. Default is `FALSE`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If `TRUE`, the `'pbapply'` package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is `TRUE`.
#'
#' @returns
#' The original `occ` data.frame with an additional column (`correct_state`)
#' indicating whether each record falls within the state specified in the
#' metadata (`TRUE`) or not (`FALSE`). As with all other flagging functions in
#' RuHere, `TRUE` indicates that the record passed this test and is eligible
#' for retention; `FALSE` indicates it failed and is flagged as potentially
#' problematic, and can be removed using `remove_flagged()`.
#'
#' @importFrom terra vect aggregate buffer is.related unwrap
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Subset occurrences for Araucaria angustifolia
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Standardize country names
#' occ_country <- standardize_countries(occ = occ,
#'                                      return_dictionary = FALSE)
#' # Standardize state names
#' occ_state <- standardize_states(occ = occ_country,
#'                                 country_column = "country_suggested",
#'                                 return_dictionary = FALSE)
#' # Check whether records fall within assigned states
#' occ_state_checked <- check_states(occ = occ_state,
#'                                     state_column = "state_suggested")
check_states <- function(occ,
                         long = "decimalLongitude",
                         lat = "decimalLatitude",
                         state_column,
                         distance = 5,
                         try_to_fix = FALSE,
                         progress_bar = FALSE,
                         verbose = TRUE) {
  # ---- ARGUMENT CHECKING ----
  is_single_str <- function(x) {
    is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  }
  is_single_bool <- function(x) {
    is.logical(x) && length(x) == 1L && !is.na(x)
  }

  # 1. Check occ
  if (missing(occ) || !inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table containing occurrence records.",
         call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # 2. Check long and lat
  if (!is_single_str(long) || !long %in% names(occ)) {
    stop(paste0("The longitude column '", long, "' was not found in 'occ'."),
         call. = FALSE)
  }
  if (!is_single_str(lat) || !lat %in% names(occ)) {
    stop(paste0("The latitude column '", lat, "' was not found in 'occ'."),
         call. = FALSE)
  }
  if (!is.numeric(occ[[long]]) || !is.numeric(occ[[lat]])) {
    stop("'long' and 'lat' columns must be numeric.", call. = FALSE)
  }

  # 3. Check state_column
  if (missing(state_column) || !is_single_str(state_column)) {
    stop("'state_column' must be a single character string with the name of the state column.",
         call. = FALSE)
  }
  if (!state_column %in% names(occ)) {
    stop(paste0("The state column '", state_column, "' was not found in 'occ'."),
         call. = FALSE)
  }

  # 4. Check distance
  if (!is.numeric(distance) || length(distance) != 1L || is.na(distance) || distance < 0) {
    stop("'distance' must be a single non-negative numeric value (in km).",
         call. = FALSE)
  }

  # 5. Check logical flags
  if (!is_single_bool(try_to_fix)) {
    stop("'try_to_fix' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }
  if (!is_single_bool(progress_bar)) {
    stop("'progress_bar' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }
  if (!is_single_bool(verbose)) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
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

  # Convert to data.frame if necessary
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  # Get unique non-NA states
  states <- unique(occ[[state_column]])
  states <- states[!is.na(states) & nzchar(trimws(as.character(states)))]

  # Get shapefile with states
  state_shp <- terra::unwrap(getExportedValue("RuHere", "states"))
  states_in <- intersect(states, state_shp$name)

  if (length(states_in) == 0L) {
    stop("None of the states listed in the '", state_column,
         "' column were found in the world map used for validation. Please check the column or run the 'standardize_states()' function.",
         call. = FALSE)
  }

  if (length(states_in) != length(states) && verbose) {
    states_out <- setdiff(states, states_in)
    warning("The following states listed in the '", state_column,
            "' column were absent in the world map used for validation: ",
            paste(states_out, collapse = ", "),
            call. = FALSE)
  }

  # Filter valid coordinates and deduplicate combinations
  valid_coords <- !is.na(occ[[long]]) & !is.na(occ[[lat]]) &
    occ[[state_column]] %in% states_in

  occ$correct_state <- NA

  if (any(valid_coords)) {
    sub_df <- occ[valid_coords, c(long, lat, state_column), drop = FALSE]
    keys <- paste(sub_df[[long]], sub_df[[lat]], sub_df[[state_column]], sep = "\r")
    uniq_idx <- !duplicated(keys)
    unique_xy <- sub_df[uniq_idx, , drop = FALSE]
    unique_keys <- keys[uniq_idx]

    # Spatialize unique valid points
    dc <- terra::vect(unique_xy,
                      geom = c(x = long, y = lat),
                      crs = "EPSG:4326")

    if (verbose) {
      message("Testing states...")
    }

    # Pre-split indices by state to avoid repeated O(N) scans inside loop
    idx_by_state <- split(seq_len(nrow(unique_xy)), unique_xy[[state_column]])
    unique_res <- rep(NA, nrow(unique_xy))
    buf_width <- distance * 1000

    res_list <- my_lapply(states_in, function(i) {
      occ_i <- idx_by_state[[i]]
      state_i <- state_shp[state_shp$name == i]
      if (buf_width > 0) {
        state_i <- terra::aggregate(terra::buffer(state_i, width = buf_width))
      }
      as.logical(terra::is.related(dc[occ_i], state_i, "intersects"))
    })

    for (k in seq_along(states_in)) {
      unique_res[idx_by_state[[states_in[k]]]] <- res_list[[k]]
    }

    # Map results back to all valid rows without altering row order
    occ$correct_state[valid_coords] <- unique_res[match(keys, unique_keys)]
  }

  # Relocate 'correct_state' right after 'state_column'
  nm <- names(occ)
  i <- match(state_column, nm)
  j <- match("correct_state", nm)
  occ <- occ[, append(nm[-j], nm[j], after = i), drop = FALSE]

  if (verbose) {
    message(sum(!occ$correct_state, na.rm = TRUE), " records fall in wrong states")
  }

  if (try_to_fix) {
    occ <- fix_states(occ = occ,
                      long = long,
                      lat = lat,
                      state_column = state_column,
                      correct_state = "correct_state",
                      distance = distance,
                      progress_bar = progress_bar,
                      verbose = verbose)
    occ$correct_state[!is.na(occ$state_issues) & occ$state_issues != "incorrect"] <- TRUE
  }

  return(occ)
}
