#' Check if the records fall in the country assigned in the metadata
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably with
#' country information standardized using `standardize_countries()`.
#' @param long (character) column name with longitude. Default is
#' 'decimalLongitude'.
#' @param lat lat (character) column name with latitude. Default is
#' 'decimalLatitude'.
#' @param country_column (character) column name containing the country
#' information.
#' @param distance (numeric) maximum distance (in kilometers) a record can fall
#' outside the country assigned in the `country_column`. Default is `5`.
#' @param try_to_fix (logical) whether to check if coordinates are inverted or
#' transposed (see `fix_countries()` for details). If `TRUE`, coordinates
#' identified as inverted or transposed will be corrected. Default is `FALSE`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) whether to print messages about function progress.
#' Default is `TRUE`.
#'
#' @returns
#' The original `occ` data.frame with an additional column (`correct_country`)
#' indicating whether each record falls within the country specified in the
#' metadata (`TRUE`) or not (`FALSE`).
#'
#' @importFrom terra vect aggregate buffer is.related unwrap
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere") #Import data example
#' # Standardize country names
#' occ_country <- standardize_countries(occ = occurrences,
#'                                      return_dictionary = FALSE)
#' # Check whether records fall within assigned countries
#' occ_country_checked <- check_countries(occ = occ_country,
#'                                        country_column = "country_suggested")
check_countries <- function(occ,
                            long = "decimalLongitude",
                            lat = "decimalLatitude",
                            country_column,
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

  # 4. Check country_column
  if (!inherits(country_column, "character") || length(country_column) != 1) {
    stop("'country_column' must be a single character string with the name of the country column.",
         call. = FALSE)
  }
  if (!country_column %in% names(occ)) {
    stop(paste0("The country column '", country_column, "' was not found in 'occ'."),
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


  #Get unique countries
  countries <- unique(occ[[country_column]])

  #Get shapefile with countries
  country_shp <- terra::unwrap(getExportedValue("RuHere", "world"))
  #Intersect with available countries to test
  countries_in <- intersect(countries, country_shp$name)

  if(length(countries_in) == 0){
    stop("None of the countries listed in the '", country_column, "' column were found in the world map used for validation. Please check the column or run the 'standardize_countries()' function.")
      }

  if(length(countries_in) != length(countries) && verbose){
    countries_out <- setdiff(countries, countries_in)
    warning("The following countries listed in the '", country_column, "' column were absent in the world map used for validation: ",
            paste(countries_out, collapse = ", "))
  }

  #Convert to data.frame if necessary
  if(inherits(occ, "data.table")){
    occ <- as.data.frame(occ)}

  #Get unique coordinates
  unique_xy <- unique(occ[, c(long, lat, country_column)])
  # Spatialize
  dc <- terra::vect(unique_xy,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")

  #Looping
  if(verbose){
    message("Testing countries...")}

  test_country <- my_sapply(countries_in, function(i){
    country_i <- country_shp[country_shp$name == i]
    # Add buffer, if necessary
    if(distance > 0){
      country_i <- terra::aggregate(terra::buffer(country_i,
                                                width = distance*1000))}
    occ_i <- which(unique_xy[[country_column]] == i)
    xy_i <- dc[occ_i]
    xy_test <- terra::is.related(xy_i, country_i, "intersects")
    names(xy_test) <- occ_i
    return(xy_test)
  })
  names(test_country) <- NULL
  test_country <- unlist(test_country)
  #Update occ of unique coordinates
  unique_xy$correct_country <- NA
  unique_xy[as.numeric(names(test_country)), "correct_country"] <- test_country

  #Merge occ again
  nm <- names(occ) # Get column names to sort columns
  # Merge
  occ <- merge(occ, unique_xy, by = c(long, lat, country_column), all.x = TRUE,
                sort = FALSE)
  # Relocate
  nm <- unique(c(nm, names(occ)))
  i <- match(country_column, nm)
  j <- match("correct_country", nm)
  new_order <- append(nm[-j], nm[j], after = i)
  occ <- occ[, new_order]

  if(verbose){
    message(sum(!occ$correct_country, na.rm = TRUE), " records fall in wrong countries")
  }

  if(try_to_fix){
    occ <- fix_countries(occ = occ,
                         long = long,
                         lat = lat,
                         country_column = country_column,
                         correct_country = "correct_country",
                         distance = distance,
                         progress_bar = progress_bar,
                         verbose = verbose)
    occ$correct_country[occ$country_issues != "incorrect"] <- TRUE
  }

  return(occ)
}
