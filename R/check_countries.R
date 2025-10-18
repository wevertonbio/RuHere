check_countries <- function(occ,
                            long = "decimalLongitude",
                            lat = "decimalLatitude",
                            country_column,
                            distance = 5,
                            try_to_fix = FALSE,
                            verbose = TRUE){
  #Get unique countries
  countries <- unique(occ[[country_column]])

  #Get shapefile with countries
  country_shp <- terra::vect(system.file("extdata/world.gpkg",
                                         package = "RuHere"))
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
  unique_xy <- occ[,c(long, lat, country_column)] %>% dplyr::distinct()
  # Spatialize
  dc <- terra::vect(unique_xy,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")

  #Looping with pbsapply
  if(verbose){
    message("Testing countries...")}

  test_country <- pbapply::pbsapply(countries_in, function(i){
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
  occ <- dplyr::left_join(occ, unique_xy, by = c(long, lat, country_column)) %>%
    dplyr::relocate(correct_country,
                    .after = dplyr::all_of(country_column))

  if(verbose){
    message(sum(!occ$correct_country, na.rm = TRUE), " records fall in wrong countries")
  }

  if(try_to_fix){
    occ <- fix_countries(occ, long, lat, country_column,
                         correct_country = "correct_country", distance, verbose)
  }

  return(occ)
}

# res2 <- check_countries(occ = res,
#                         country_column = "country_suggested",
#                         try_to_fix = TRUE)


# occ <- res
# long = "decimalLongitude"
# lat = "decimalLatitude"
# country_column = "country_suggested"
# distance = 5
# verbose = T
