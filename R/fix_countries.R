#' Identify and correct coordinates based on country information
#'
#' @description
#' This function identifies and correct inverted and transposed coordinates
#' based on country information
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably with
#' country information checked using `check_countries()`.
#' @param long (character) column name with longitude. Default is
#' 'decimalLongitude'.
#' @param lat lat (character) column name with latitude. Default is
#' 'decimalLatitude'.
#' @param country_column (character) name of the column containing the country
#' information.
#' @param correct_country (character) name of the column with logical value
#' indicating whether each record falls within the country specified in the
#' metadata. Default is 'correct_country'. See details.
#' @param distance (numeric) maximum distance (in kilometers) a record can fall
#' outside the country assigned in the `country_column`. Default is `5`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is TRUE.
#'
#' @details
#' The function checks and corrects coordinate errors in occurrence records
#' by testing whether each point falls within the expected country polygon
#' (from `RuHere`’s internal world map).
#'
#' The input occurrence data must contain a column (specified in the
#' `correct_country` argument) with logical values indicating which records to
#' check and fix — only those marked as FALSE will be processed. This column
#' can be obtained by running the `check_countries()` function.
#'
#' It runs a series of seven tests to detect common issues such as **inverted**
#' signs or **swapped** latitude/longitude values. Inverted coordinates have their
#' signs flipped (e.g., -45 instead of 45), placing the point in the opposite
#' hemisphere, while swapped coordinates have latitude and longitude values
#' exchanged (e.g., -47, -15 instead of -15, -47).
#'
#' For each test, country borders are buffered by `distance` km to account for
#' minor positional errors.
#'
#' The type of issue (or `"correct"`) is recorded in a new column,
#' `country_issues`. Records that match their assigned country after any
#' correction are updated accordingly, while remaining mismatches are labeled
#' `"incorrect"`.
#'
#' This function can be used internally by `check_countries()` to automatically
#' identify and fix common coordinate errors.
#'
#' @returns
#' The original `occ` data.frame with the coordinates in the `long` and `lat`
#' columns corrected, and an additional column (`country_issues`) indicating
#' whether the coordinates are:
#' - **correct**: the record falls within the assigned country;
#' - **inverted**: longitude and/or latitude have reversed signs;
#' - **swapped**: longitude and latitude are transposed (i.e., each appears in
#' the other's column).
#' **incorrect**: the record falls outside the assigned country and could not
#' be corrected.
#'
#' @importFrom terra vect buffer is.related
#' @importFrom pbapply pbsapply
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere") # Import example data
#'
#' # Standardize country names
#' occ_country <- standardize_countries(occ = occurrences,
#'                                      return_dictionary = FALSE)
#'
#' # Check whether records fall within the assigned countries
#' occ_country_checked <- check_countries(occ = occ_country,
#'                                        country_column = "country_suggested")
#'
#' # Fix records with incorrect or misassigned countries
#' occ_country_fixed <- fix_countries(occ = occ_country_checked,
#'                                    country_column = "country_suggested")
fix_countries <- function(occ,
                          long = "decimalLongitude",
                          lat = "decimalLatitude",
                          country_column,
                          correct_country = "correct_country",
                          distance = 5,
                          verbose = TRUE){
  # ---- ARGUMENT CHECKING ----

  # 1. Check occ
  if (missing(occ) || !inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table containing occurrence records.",
         call. = FALSE)
  }
  if (nrow(occ) == 0) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.",
         call. = FALSE)
  }

  # 2. Check long
  if (!is.character(long) || length(long) != 1) {
    stop("'long' must be a single character string specifying the longitude column.",
         call. = FALSE)
  }
  if (!long %in% names(occ)) {
    stop(paste0("The column specified in 'long' ('", long, "') was not found in 'occ'."),
         call. = FALSE)
  }

  # 3. Check lat
  if (!is.character(lat) || length(lat) != 1) {
    stop("'lat' must be a single character string specifying the latitude column.",
         call. = FALSE)
  }
  if (!lat %in% names(occ)) {
    stop(paste0("The column specified in 'lat' ('", lat, "') was not found in 'occ'."),
         call. = FALSE)
  }

  # 4. Check country_column
  if (!is.character(country_column) || length(country_column) != 1) {
    stop("'country_column' must be a single character string specifying the country column.",
         call. = FALSE)
  }
  if (!country_column %in% names(occ)) {
    stop(paste0("The column specified in 'country_column' ('", country_column, "') was not found in 'occ'."),
         call. = FALSE)
  }

  # 5. Check correct_country
  if (!is.character(correct_country) || length(correct_country) != 1) {
    stop("'correct_country' must be a single character string specifying the logical column with country validation results.",
         call. = FALSE)
  }
  if (!correct_country %in% names(occ)) {
    stop(paste0("The column specified in 'correct_country' ('", correct_country, "') was not found in 'occ'. ",
                "It can be created by running `check_countries()`."),
         call. = FALSE)
  }
  if (!inherits(occ[[correct_country]], "logical")) {
    stop(paste0("The column '", correct_country, "' must contain logical values (TRUE/FALSE)."),
         call. = FALSE)
  }

  # 6. Check distance
  if (!inherits(distance, "numeric") || length(distance) != 1 || distance < 0) {
    stop("'distance' must be a single non-negative numeric value (in kilometers).",
         call. = FALSE)
  }

  # 7. Check verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }


  #Convert to data.frame if necessary
  if(inherits(occ, c("data.table", "tbl_df"))){
    occ <- as.data.frame(occ)}

  #Subset occ
  filter_column <- occ[[correct_country]]
  occ_correct <- occ[filter_column | is.na(filter_column), ]
  occ_incorrect <- occ[!filter_column & !is.na(filter_column), ]

  #Create columns to identify problems
  occ_correct[["country_issues"]] <- "correct"
  occ_incorrect[["country_issues"]] <- "incorrect"

  #Get shapefile with countries
  country_shp <- terra::vect(system.file("extdata/world.shp",
                                         package = "RuHere"))

  #Get all countries
  all_countries <- unique(country_shp$name)

  ####1 - Inverted signal of longitude####
  d1 <- occ_incorrect[,c(long,lat, country_column)]
  countries_1 <- base::intersect(unique(d1[[country_column]]), all_countries)

  d1[, long] <- - d1[,long]
  d1 <- terra::vect(d1,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")
  if(verbose)
    message("Task 1 of 7: testing if longitude is inverted")
  test_1 <- pbapply::pbsapply(countries_1, function(i){
    country_i <- country_shp[country_shp$name == i]
    if(distance > 0){
      country_i <- terra::buffer(country_i, width = distance*1000)}
    occ_i <- which(d1[[country_column]] == i)
    xy_i <- d1[occ_i]
    xy_test <- terra::is.related(xy_i, country_i, "intersects")
    names(xy_test) <- occ_i
    return(xy_test)
  })
  names(test_1) <- NULL
  test_1 <- unlist(test_1)
  if(verbose)
    message(sum(test_1), " coordinates with longitude inverted")
  test_1 <- ifelse(test_1, "correct", "incorrect")

  #Update occ
  occ_incorrect[as.numeric(names(test_1)), "country_issues"] <- test_1

  #Update coordinates
  occ_incorrect[,long][
    occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,long][
      occ_incorrect[,"country_issues"] == "correct"]
  #Update transposed information
  occ_incorrect[, "country_issues"][
    occ_incorrect[, "country_issues"] == "correct"] <- "inverted_long"

  #Update correct and incorrect occ
  occ_correct <- rbind(occ_correct,
                       occ_incorrect[occ_incorrect$country_issues != "incorrect", ])
  occ_incorrect <- occ_incorrect[occ_incorrect$country_issues == "incorrect",]


  ####2 - Inverted signal of latitude ####
  if(nrow(occ_incorrect) > 0) {
    d2 <- occ_incorrect[,c(long,lat, country_column)]
    countries_2 <- intersect(unique(d2[,country_column]), all_countries)
    d2[, lat] <- - d2[,lat]
    d2 <- terra::vect(d2,
                      geom = c(x = long, y = lat),
                      crs = "+init=epsg:4326")
    if(verbose)
      message("Task 2 of 7: testing if latitude is inverted")
    test_2 <- pbapply::pbsapply(countries_2, function(i){
      country_i <- country_shp[country_shp$name== i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      occ_i <- which(d2[[country_column]] == i)
      xy_i <- d2[occ_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_2) <- NULL
    test_2 <- unlist(test_2)
    if(verbose)
      message(sum(test_2), " coordinates with latitude inverted")

    test_2 <- ifelse(test_2, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_2)), "country_issues"] <- test_2

    #Update coordinates
    occ_incorrect[,lat][
      occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"country_issues"] == "correct"]
    #Update transposed information
    occ_incorrect[, "country_issues"][
      occ_incorrect[, "country_issues"] == "correct"] <- "inverted_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$country_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$country_issues == "incorrect",]
  }

  ####3 - Inverted signal of longitude and latitude ####
  if(nrow(occ_incorrect) > 0) {
    d3 <- occ_incorrect[,c(long,lat, country_column)]
    countries_3 <- intersect(unique(d3[,country_column]), all_countries)
    d3[, long] <- - d3[,long]
    d3[, lat] <- - d3[,lat]
    d3 <- terra::vect(d3,
                      geom = c(x = long, y = lat),
                      crs = "+init=epsg:4326")
    if(verbose)
      message("Task 3 of 7: testing if longitude and latitude are inverted")
    test_3 <- pbapply::pbsapply(countries_3, function(i){
      country_i <- country_shp[country_shp$name == i]
      if(distance > 0){
        country_i <- terra::buffer(country_i, width = distance*1000)}
      occ_i <- which(d3[[country_column]] == i)
      xy_i <- d3[occ_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_3) <- NULL
    test_3 <- unlist(test_3)
    if(verbose)
      message(sum(test_3), " coordinates with longitude and latitude inverted")

    test_3 <- ifelse(test_3, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_3)), "country_issues"] <- test_3

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,long][
        occ_incorrect[,"country_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"country_issues"] == "correct"]
    #Update transposed information
    occ_incorrect[, "country_issues"][
      occ_incorrect[, "country_issues"] == "correct"] <- "inverted_long_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$country_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$country_issues == "incorrect",]
  }

  ####4 - Swap longitude and latitude ####
  if(nrow(occ_incorrect) > 0) {
    d4 <- occ_incorrect[,c(long,lat, country_column)]
    countries_4 <- intersect(unique(d4[,country_column]), all_countries)
    d4 <- terra::vect(d4,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    if(verbose)
      message("Task 4 of 7: testing if longitude and latitude are swapped")
    test_4 <- pbapply::pbsapply(countries_4, function(i){
      country_i <- country_shp[country_shp$name == i]
      if(distance > 0){
        country_i <- terra::buffer(country_i, width = distance*1000)}
      occ_i <- which(d4[[country_column]] == i)
      xy_i <- d4[occ_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_4) <- NULL
    test_4 <- unlist(test_4)
    if(verbose)
      message(sum(test_4), " coordinates with longitude and latitude swapped")
    test_4 <- ifelse(test_4, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_4)), "country_issues"] <- test_4

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"country_issues"] == "correct"] <- occ_incorrect[,lat][
        occ_incorrect[,"country_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"country_issues"] == "correct"] <- occ_incorrect[,long][
        occ_incorrect[,"country_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "country_issues"][
      occ_incorrect[, "country_issues"] == "correct"] <- "swapped_long_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$country_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$country_issues == "incorrect",]
  }

  ####5 - Swap longitude and latitude, with latitude inverted ####
  if(nrow(occ_incorrect) > 0) {
    d5 <- occ_incorrect[,c(long,lat, country_column)]
    countries_5 <- intersect(unique(d5[,country_column]), all_countries)
    d5[, long] <- - d5[,long]
    # d5[, lat] <- - d5[,lat]
    d5 <- terra::vect(d5,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    if(verbose)
      message("Task 5 of 7: testing if longitude and latitude are swapped -
            with longitude inverted")
    test_5 <- pbapply::pbsapply(countries_5, function(i){
      country_i <- country_shp[country_shp$name == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      occ_i <- which(d5[[country_column]] == i)
      xy_i <- d5[occ_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_5) <- NULL
    test_5 <- unlist(test_5)
    if(verbose)
      message(sum(test_5), " coordinates with longitude and latitude swapped and latitude inverted")
    test_5 <- ifelse(test_5, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_5)), "country_issues"] <- test_5

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"country_issues"] == "correct"] <- occ_incorrect[,lat][
      occ_incorrect[,"country_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,long][
      occ_incorrect[,"country_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "country_issues"][
      occ_incorrect[, "country_issues"] == "correct"] <- "swapped_and_inverted_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$country_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$country_issues == "incorrect",]
    }

  ####6 - Swap longitude and latitude, with longitude inverted ####
  if(nrow(occ_incorrect) > 0) {
    d6 <- occ_incorrect[,c(long,lat, country_column)]
    countries_6 <- intersect(unique(d6[,country_column]), all_countries)
    #d6[, long] <- - d6[,long]
    d6[, lat] <- - d6[,lat]
    d6 <- terra::vect(d6,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    if(verbose)
      message("Task 6 of 7: testing if longitude and latitude are swapped - with latitude inverted")
    test_6 <- pbapply::pbsapply(countries_6, function(i){
      country_i <- country_shp[country_shp$name == i]
      if(distance > 0){
        country_i <- terra::buffer(country_i, width = distance*1000)}
      occ_i <- which(d6[[country_column]] == i)
      xy_i <- d6[occ_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_6) <- NULL
    test_6 <- unlist(test_6)
    if(verbose)
      message(sum(test_6), " coordinates with longitude and latitude swapped and longitude inverted")
    test_6 <- ifelse(test_6, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_6)), "country_issues"] <- test_6

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"country_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"country_issues"] == "correct"] <- occ_incorrect[,long][
        occ_incorrect[,"country_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "country_issues"][
      occ_incorrect[, "country_issues"] == "correct"] <- "swapped_and_inverted_long"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$country_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$country_issues == "incorrect",]
  }

  ####7 - Swap longitude and latitude, with latitude and longitude inverted ####
  if(nrow(occ_incorrect) > 0) {
    d7 <- occ_incorrect[,c(long,lat, country_column)]
    countries_7 <- intersect(unique(d7[,country_column]), all_countries)
    d7[, long] <- - d7[,long]
    d7[, lat] <- - d7[,lat]
    d7 <- terra::vect(d7,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    if(verbose)
      message("Task 7 of 7: testing if longitude and latitude are swapped - with longitude latitude inverted")
    test_7 <- pbapply::pbsapply(countries_7, function(i){
      #print(i)
      country_i <- country_shp[country_shp$name == i]
      country_i <- terra::buffer(country_i, width = distance*1000)
      occ_i <- which(d7[[country_column]] == i)
      xy_i <- d7[occ_i]
      xy_test <- terra::is.related(xy_i, country_i, "intersects")
      names(xy_test) <- occ_i
      return(xy_test)
    })
    names(test_7) <- NULL
    test_7 <- unlist(test_7)
    if(verbose)
      message(sum(test_7), " coordinates with longitude and latitude swapped and inverted")
    test_7 <- ifelse(test_7, "correct", "incorrect")

    #Update occ
    occ_incorrect[as.numeric(names(test_7)), "country_issues"] <- test_7

    #Update coordinates
    occ_incorrect[,long][
      occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,lat][
        occ_incorrect[,"country_issues"] == "correct"]
    occ_incorrect[,lat][
      occ_incorrect[,"country_issues"] == "correct"] <- - occ_incorrect[,long][
        occ_incorrect[,"country_issues"] == "correct"]

    #Update transposed information
    occ_incorrect[, "country_issues"][
      occ_incorrect[, "country_issues"] == "correct"] <- "swapped_and_inverted_long_lat"

    #Update correct and incorrect occ
    occ_correct <- rbind(occ_correct,
                         occ_incorrect[occ_incorrect$country_issues != "incorrect", ])
    occ_incorrect <- occ_incorrect[occ_incorrect$country_issues == "incorrect",]
  }

  #Final occset
  if(nrow(occ_incorrect) == 0) {
    occ <- occ_correct
  } else {
    occ <- rbind(occ_correct, occ_incorrect)
  }

  # Reorder columns
  others <- setdiff(names(occ), "country_issues")

  # rebuild order:
  ref_pos <- match(correct_country, others)

  new_names <- append(others, "country_issues", after = ref_pos)

  occ <- occ[, new_names]

  return(occ)
}
