#' Identify and correct coordinates based on state information
#'
#' @description
#' This function identifies and correct inverted and transposed coordinates
#' based on state information.
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably with
#' state information checked using `state_countries()`.
#' @param long (character) column name with longitude. Default is
#' 'decimalLongitude'.
#' @param lat lat (character) column name with latitude. Default is
#' 'decimalLatitude'.
#' @param state_column (character) name of the column containing the state
#' information.
#' @param correct_state (character) name of the column with logical value
#' indicating whether each record falls within the state specified in the
#' metadata. Default is 'correct_state'. See details.
#' @param distance (numeric) maximum distance (in kilometers) a record can fall
#' outside the state assigned in the `state_column`. Default is `5`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is TRUE.
#'
#' @details
#' The function checks and corrects coordinate errors in occurrence records
#' by testing whether each point falls within the expected state polygon
#' (from `RuHere`’s internal world map).
#'
#' The input occurrence data must contain a column (specified in the
#' `correct_state` argument) with logical values indicating which records to
#' check and fix — only those marked as FALSE will be processed. This column
#' can be obtained by running the `check_states()` function.
#'
#' It runs a series of seven tests to detect common issues such as **inverted**
#' signs or **swapped** latitude/longitude values. Inverted coordinates have
#' their signs flipped (e.g., -45 instead of 45), placing the point in the
#' opposite hemisphere, while swapped coordinates have latitude and longitude
#' values exchanged (e.g., -47, -15 instead of -15, -47).
#'
#' For each test, state borders are buffered by `distance` km to account for
#' minor positional errors.
#'
#' The type of issue (or `"correct"`) is recorded in a new column,
#' `state_issues`. Records that match their assigned state after any
#' correction are updated accordingly, while remaining mismatches are labeled
#' `"incorrect"`.
#'
#' This function can be used internally by `check_states()` to automatically
#' identify and fix common coordinate errors.
#'
#' @returns
#' The original `occ` data.frame with the coordinates in the `long` and `lat`
#' columns corrected, and an additional column (`state_issues`) indicating
#' whether the coordinates are:
#' - **correct**: the record falls within the assigned state;
#' - **inverted**: longitude and/or latitude have reversed signs;
#' - **swapped**: longitude and latitude are transposed (i.e., each appears in
#' the other's column).
#' **incorrect**: the record falls outside the assigned state and could not
#' be corrected.
#'
#' @export
#'
#' @importFrom terra vect buffer is.related unwrap
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere") # Import example data
#' # Subset records of Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Standardize country names
#' occ_country <- standardize_countries(occ = occ,
#'                                      return_dictionary = FALSE)
#'
#' # Standardize state names
#' occ_state <- standardize_states(occ = occ_country,
#'                                 country_column = "country_suggested",
#'                                 return_dictionary = FALSE)
#'
#' # Check whether records fall within the assigned states
#' occ_states_checked <- check_states(occ = occ_state,
#'                                    state_column = "state_suggested")
#'
#' # Fix records with incorrect or misassigned states
#' occ_states_fixed <- fix_states(occ = occ_states_checked,
#'                                state_column = "state_suggested")
fix_states <- function(occ,
                       long = "decimalLongitude",
                       lat = "decimalLatitude",
                       state_column,
                       correct_state = "correct_state",
                       distance = 5,
                       progress_bar = FALSE,
                       verbose = TRUE){

  # ---- ARGUMENT CHECKING ----

  # 1. Check occ
  if (!inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame containing occurrence records.",
         call. = FALSE)
  }
  if (nrow(occ) == 0) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # 2. Check long
  if (!inherits(long, "character") || length(long) != 1) {
    stop("'long' must be a single character string specifying the longitude column name.",
         call. = FALSE)
  }
  if (!long %in% names(occ)) {
    stop(paste0("The column specified in 'long' ('", long, "') was not found in 'occ'."),
         call. = FALSE)
  }

  # 3. Check lat
  if (!inherits(lat, "character") || length(lat) != 1) {
    stop("'lat' must be a single character string specifying the latitude column name.",
         call. = FALSE)
  }
  if (!lat %in% names(occ)) {
    stop(paste0("The column specified in 'lat' ('", lat, "') was not found in 'occ'."),
         call. = FALSE)
  }

  # 4. Check state_column
  if (!inherits(state_column, "character") || length(state_column) != 1) {
    stop("'state_column' must be a single character string specifying the column with state information.",
         call. = FALSE)
  }
  if (!state_column %in% names(occ)) {
    stop(paste0("The column specified in 'state_column' ('", state_column, "') was not found in 'occ'."),
         call. = FALSE)
  }

  # 5. Check correct_state
  if (!inherits(correct_state, "character") || length(correct_state) != 1) {
    stop("'correct_state' must be a single character string specifying the column with logical values indicating valid records.",
         call. = FALSE)
  }
  if (!correct_state %in% names(occ)) {
    stop(paste0("The column specified in 'correct_state' ('", correct_state, "') was not found in 'occ'. ",
                "You can obtain this column by running `check_states()`."),
         call. = FALSE)
  }
  if (!inherits(occ[[correct_state]], "logical")) {
    stop(paste0("The column specified in 'correct_state' ('", correct_state, "') must contain logical (TRUE/FALSE) values."),
         call. = FALSE)
  }

  # 6. Check distance
  if (!inherits(distance, "numeric") || length(distance) != 1 ||
      distance < 0 || is.na(distance)) {
    stop("'distance' must be a single non-negative numeric value (in kilometers).",
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
  if(inherits(occ, c("data.table", "tbl_df"))){
    occ <- as.data.frame(occ)}

  #Subset occ
  filter_column <- occ[[correct_state]]
  occ_correct <- occ[filter_column | is.na(filter_column), ]
  occ_incorrect <- occ[!filter_column & !is.na(filter_column), ]

  #Create columns to identify problems
  occ_correct[["state_issues"]] <- "correct"
  occ_incorrect[["state_issues"]] <- "incorrect"

  #Get shapefile with states
  state_shp <- terra::unwrap(getExportedValue("RuHere", "states"))

  #Get all states
  all_states <- unique(state_shp$name)

  ####1 - Inverted signal of longitude####
  d1 <- occ_incorrect[,c(long,lat, state_column)]
  states_1 <- base::intersect(unique(d1[,state_column]), all_states)

  d1[, long] <- - d1[,long]
  d1 <- terra::vect(d1,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")
  message("Task 1 of 7: testing if longitude is inverted")
  test_1 <- my_sapply(states_1, function(i){
    state_i <- state_shp[state_shp$name == i]
    if(distance > 0){
      state_i <- terra::buffer(state_i, width = distance*1000)}
    occ_i <- which(d1[[state_column]] == i)
    xy_i <- d1[occ_i]
    xy_test <- terra::is.related(xy_i, state_i, "intersects")
    names(xy_test) <- occ_i
    return(xy_test)
  })
  names(test_1) <- NULL
  test_1 <- unlist(test_1)
  message(sum(test_1), " coordinates with longitude inverted")
  test_1 <- ifelse(test_1, "correct", "incorrect")

  #Update occ
  occ_incorrect[as.numeric(names(test_1)), "state_issues"] <- test_1

  #Update coordinates
  occ_incorrect[,long][
    occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,long][
      occ_incorrect[,"state_issues"] == "correct"]
  #Update transposed information
  occ_incorrect[, "state_issues"][
    occ_incorrect[, "state_issues"] == "correct"] <- "inverted_long"

  #Update correct and incorrect occ
  occ_correct <- rbind(occ_correct,
                       occ_incorrect[occ_incorrect$state_issues != "incorrect", ])
  occ_incorrect <- occ_incorrect[occ_incorrect$state_issues == "incorrect",]

  ####2 - Inverted signal of latitude ####
  if(nrow(occ_incorrect) > 0) {
    d2 <- occ_incorrect[,c(long,lat, state_column)]
    states_2 <- intersect(unique(d2[,state_column]), all_states)
    d2[, lat] <- - d2[,lat]
    d2 <- terra::vect(d2,
                      geom = c(x = long, y = lat),
                      crs = "+init=epsg:4326")
    message("Task 2 of 7: testing if latitude is inverted")
    test_2 <- my_sapply(states_2, function(i){
      state_i <- state_shp[state_shp$name== i]
      state_i <- terra::buffer(state_i, width = distance*1000)
      occ_i <- which(d2[[state_column]] == i)
      xy_i <- d2[occ_i]
      xy_test <- terra::is.related(xy_i, state_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_2) <- NULL
    test_2 <- unlist(test_2)
    message(sum(test_2), " coordinates with latitude inverted")

    test_2 <- ifelse(test_2, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_2)), "state_issues"] <- test_2

    #Update coordinates
    occ_incorrect[,lat][
      occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"state_issues"] == "correct"]
    #Update transposed information
    occ_incorrect[, "state_issues"][
      occ_incorrect[, "state_issues"] == "correct"] <- "inverted_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$state_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$state_issues == "incorrect",]
  }

  ####3 - Inverted signal of longitude and latitude ####
  if(nrow(occ_incorrect) > 0) {
    d3 <- occ_incorrect[,c(long,lat, state_column)]
    states_3 <- intersect(unique(d3[,state_column]), all_states)
    d3[, long] <- - d3[,long]
    d3[, lat] <- - d3[,lat]
    d3 <- terra::vect(d3,
                      geom = c(x = long, y = lat),
                      crs = "+init=epsg:4326")
    message("Task 3 of 7: testing if longitude and latitude are inverted")
    test_3 <- my_sapply(states_3, function(i){
      state_i <- state_shp[state_shp$name == i]
      if(distance > 0){
        state_i <- terra::buffer(state_i, width = distance*1000)}
      occ_i <- which(d3[[state_column]] == i)
      xy_i <- d3[occ_i]
      xy_test <- terra::is.related(xy_i, state_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_3) <- NULL
    test_3 <- unlist(test_3)
    message(sum(test_3), " coordinates with longitude and latitude inverted")

    test_3 <- ifelse(test_3, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_3)), "state_issues"] <- test_3

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,long][
        occ_incorrect[,"state_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"state_issues"] == "correct"]
    #Update transposed information
    occ_incorrect[, "state_issues"][
      occ_incorrect[, "state_issues"] == "correct"] <- "inverted_long_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$state_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$state_issues == "incorrect",]
  }

  ####4 - Swap longitude and latitude ####
  if(nrow(occ_incorrect) > 0) {
    d4 <- occ_incorrect[,c(long,lat, state_column)]
    states_4 <- intersect(unique(d4[,state_column]), all_states)
    d4 <- terra::vect(d4,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 4 of 7: testing if longitude and latitude are swapped")
    test_4 <- my_sapply(states_4, function(i){
      state_i <- state_shp[state_shp$name == i]
      if(distance > 0){
        state_i <- terra::buffer(state_i, width = distance*1000)}
      occ_i <- which(d4[[state_column]] == i)
      xy_i <- d4[occ_i]
      xy_test <- terra::is.related(xy_i, state_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_4) <- NULL
    test_4 <- unlist(test_4)
    message(sum(test_4), " coordinates with longitude and latitude swapped")
    test_4 <- ifelse(test_4, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_4)), "state_issues"] <- test_4

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"state_issues"] == "correct"] <- occ_incorrect[,lat][
        occ_incorrect[,"state_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"state_issues"] == "correct"] <- occ_incorrect[,long][
        occ_incorrect[,"state_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "state_issues"][
      occ_incorrect[, "state_issues"] == "correct"] <- "swapped_long_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$state_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$state_issues == "incorrect",]
  }

  ####5 - Swap longitude and latitude, with latitude inverted ####
  if(nrow(occ_incorrect) > 0) {
    d5 <- occ_incorrect[,c(long,lat, state_column)]
    states_5 <- intersect(unique(d5[,state_column]), all_states)
    d5[, long] <- - d5[,long]
    # d5[, lat] <- - d5[,lat]
    d5 <- terra::vect(d5,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 5 of 7: testing if longitude and latitude are swapped -
            with latitude inverted")
    test_5 <- my_sapply(states_5, function(i){
      state_i <- state_shp[state_shp$name == i]
      state_i <- terra::buffer(state_i, width = distance*1000)
      occ_i <- which(d5[[state_column]] == i)
      xy_i <- d5[occ_i]
      xy_test <- terra::is.related(xy_i, state_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_5) <- NULL
    test_5 <- unlist(test_5)
    message(sum(test_5), " coordinates with longitude and latitude swapped and latitude inverted")
    test_5 <- ifelse(test_5, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_5)), "state_issues"] <- test_5

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"state_issues"] == "correct"] <- occ_incorrect[,lat][
        occ_incorrect[,"state_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,long][
        occ_incorrect[,"state_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "state_issues"][
      occ_incorrect[, "state_issues"] == "correct"] <- "swapped_and_inverted_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$state_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$state_issues == "incorrect",]
  }

  ####6 - Swap longitude and latitude, with longitude inverted ####
  if(nrow(occ_incorrect) > 0) {
    d6 <- occ_incorrect[,c(long,lat, state_column)]
    states_6 <- intersect(unique(d6[,state_column]), all_states)
    #d6[, long] <- - d6[,long]
    d6[, lat] <- - d6[,lat]
    d6 <- terra::vect(d6,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 6 of 7: testing if longitude and latitude are swapped - with latitude inverted")
    test_6 <- my_sapply(states_6, function(i){
      state_i <- state_shp[state_shp$name == i]
      if(distance > 0){
        state_i <- terra::buffer(state_i, width = distance*1000)}
      occ_i <- which(d6[[state_column]] == i)
      xy_i <- d6[occ_i]
      xy_test <- terra::is.related(xy_i, state_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_6) <- NULL
    test_6 <- unlist(test_6)
    message(sum(test_6), " coordinates with longitude and latitude swapped and longitude inverted")
    test_6 <- ifelse(test_6, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_6)), "state_issues"] <- test_6

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"state_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"state_issues"] == "correct"] <- occ_incorrect[,long][
        occ_incorrect[,"state_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "state_issues"][
      occ_incorrect[, "state_issues"] == "correct"] <- "swapped_and_inverted_long"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$state_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$state_issues == "incorrect",]
  }

  ####7 - Swap longitude and latitude, with latitude and longitude inverted ####
  if(nrow(occ_incorrect) > 0) {
    d7 <- occ_incorrect[,c(long,lat, state_column)]
    states_7 <- intersect(unique(d7[,state_column]), all_states)
    d7[, long] <- - d7[,long]
    d7[, lat] <- - d7[,lat]
    d7 <- terra::vect(d7,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 7 of 7: testing if longitude and latitude are swapped - with longitude latitude inverted")
    test_7 <- my_sapply(states_7, function(i){
      #print(i)
      state_i <- state_shp[state_shp$name == i]
      state_i <- terra::buffer(state_i, width = distance*1000)
      occ_i <- which(d7[[state_column]] == i)
      xy_i <- d7[occ_i]
      xy_test <- terra::is.related(xy_i, state_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_7) <- NULL
    test_7 <- unlist(test_7)
    message(sum(test_7), " coordinates with longitude and latitude swapped and inverted")
    test_7 <- ifelse(test_7, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_7)), "state_issues"] <- test_7

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"state_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"state_issues"] == "correct"] <- - occ_incorrect[,long][
        occ_incorrect[,"state_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "state_issues"][
      occ_incorrect[, "state_issues"] == "correct"] <- "swapped_and_inverted_long_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$state_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$state_issues == "incorrect",]
  }

  #Final occset
  if(nrow(occ_incorrect) == 0) {
    occ <- occ_correct
  } else {
    occ <- rbind(occ_correct, occ_incorrect)
  }

  # Reorder columns
  others <- setdiff(names(occ), "state_issues")

  # rebuild order:
  ref_pos <- match(correct_state, others)

  new_names <- append(others, "state_issues", after = ref_pos)

  occ <- occ[, new_names]

  return(occ)
}
