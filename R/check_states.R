#' Check if the records fall in the state assigned in the metadata
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably with
#' country information standardized using `standardize_states()`.
#' @param long (character) column name with longitude. Default is
#' 'decimalLongitude'.
#' @param lat lat (character) column name with latitude. Default is
#' 'decimalLatitude'.
#' @param state_column (character) column name containing the state information.
#' @param distance (numeric) maximum distance (in kilometers) a record can fall
#' outside the state assigned in the `state_column`. Default is `5`.
#' @param try_to_fix (logical) whether to check if coordinates are inverted or
#' transposed (see `fix_states()` for details). If `TRUE`, coordinates
#' identified as inverted or transposed will be corrected. Default is `FALSE`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is `TRUE`.
#'
#' @returns
#' The original `occ` data.frame with an additional column (`correct_state`)
#' indicating whether each record falls within the state specified in the
#' metadata (`TRUE`) or not (`FALSE`).
#'
#' @importFrom terra vect aggregate buffer is.related unwrap
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere") #Import data example
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
                         verbose = TRUE){

  # ---- ARGUMENT CHECKING ----

  # 1. Check occ
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table containing occurrence records.",
         call. = FALSE)
  }
  if (nrow(occ) == 0) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # 2. Check long
  if (!inherits(long, "character") || length(long) != 1) {
    stop("'long' must be a single character string with the name of the longitude column.",
         call. = FALSE)
  }
  if (!long %in% names(occ)) {
    stop(paste0("The longitude column '", long, "' was not found in 'occ'."),
         call. = FALSE)
  }

  # 3. Check lat
  if (!inherits(lat, "character") || length(lat) != 1) {
    stop("'lat' must be a single character string with the name of the latitude column.",
         call. = FALSE)
  }
  if (!lat %in% names(occ)) {
    stop(paste0("The latitude column '", lat, "' was not found in 'occ'."),
         call. = FALSE)
  }

  # 4. Check state_column
  if (!inherits(state_column, "character") || length(state_column) != 1) {
    stop("'state_column' must be a single character string with the name of the state column.",
         call. = FALSE)
  }
  if (!state_column %in% names(occ)) {
    stop(paste0("The state column '", state_column, "' was not found in 'occ'."),
         call. = FALSE)
  }

  # 5. Check distance
  if (!inherits(distance, "numeric") || length(distance) != 1 || distance < 0) {
    stop("'distance' must be a single non-negative numeric value (in km).",
         call. = FALSE)
  }

  # 6. Check try_to_fix
  if (!inherits(try_to_fix, "logical") || length(try_to_fix) != 1) {
    stop("'try_to_fix' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # 7. Check verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # progress_bar
  if (!inherits(progress_bar, "logical") || length(progress_bar) != 1) {
    stop("'progress_bar' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  # progress_bar
  if (!inherits(progress_bar, "logical") || length(progress_bar) != 1) {
    stop("'progress_bar' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }


  if (progress_bar) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      my_sapply <- pbapply::pbsapply
    } else {
      stop("Package 'pbapply' is required if 'progress_bar = TRUE'.
Run install.packages('pbapply')", call. = FALSE)
    }
  } else {
    my_sapply <- base::sapply
  }

  #Convert to data.frame if necessary
  if(inherits(occ, "data.table")){
    occ <- as.data.frame(occ)}

  #Get unique states
  states <- unique(occ[[state_column]])

  #Get shapefile with states
  state_shp <- terra::unwrap(getExportedValue("RuHere", "states"))
  #Intersect with available states to test
  states_in <- intersect(states, state_shp$name)

  if(length(states_in) == 0){
    stop("None of the states listed in the '", state_column, "' column were found in the world map used for validation. Please check the column or run the 'standardize_states()' function.")
  }

  if(length(states_in) != length(states) && verbose){
    states_out <- setdiff(states, states_in)
    warning("The following states listed in the '", state_column, "' column were absent in the world map used for validation: ",
            paste(states_out, collapse = ", "))
  }

  #Get unique coordinates
  unique_xy <- unique(occ[,c(long, lat, state_column)])
  # Spatialize
  dc <- terra::vect(unique_xy,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")

  #Looping
  if(verbose){
    message("Testing states...")}

  test_state <- my_sapply(states_in, function(i){
    state_i <- state_shp[state_shp$name == i]
    # Add buffer, if necessary
    if(distance > 0){
      state_i <- terra::aggregate(terra::buffer(state_i,
                                                  width = distance*1000))}
    occ_i <- which(unique_xy[[state_column]] == i)
    xy_i <- dc[occ_i]
    xy_test <- terra::is.related(xy_i, state_i, "intersects")
    names(xy_test) <- occ_i
    return(xy_test)
  })
  names(test_state) <- NULL
  test_state <- unlist(test_state)
  #Update occ of unique coordinates
  unique_xy$correct_state <- NA
  unique_xy[as.numeric(names(test_state)), "correct_state"] <- test_state

  #Merge occ again
  #Merge occ again
  occ <- merge(occ, unique_xy, by = c(long, lat, state_column), all.x = TRUE,
               sort = FALSE)
  # Relocate
  nm <- names(occ)
  i <- match(state_column, nm)
  j <- match("correct_state", nm)
  new_order <- append(nm[-j], nm[j], after = i)
  occ <- occ[, new_order]

  if(verbose){
    message(sum(!occ$correct_state, na.rm = TRUE), " records fall in wrong states")
  }

  if(try_to_fix){
    occ <- fix_states(occ, long, lat, state_column,
                      correct_state = "correct_state", distance, verbose)
    occ$correct_state[occ$state_issues != "incorrect"] <- TRUE
  }

  return(occ)
}
