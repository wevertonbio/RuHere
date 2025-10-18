check_states <- function(occ,
                         long = "decimalLongitude",
                         lat = "decimalLatitude",
                         state_column,
                         distance = 5,
                         try_to_fix = FALSE,
                         verbose = TRUE){
  #Get unique states
  states <- unique(occ[[state_column]])

  #Get shapefile with states
  state_shp <- terra::vect(system.file("extdata/states.gpkg",
                                         package = "RuHere"))
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

  #Convert to data.frame if necessary
  if(inherits(occ, "data.table")){
    occ <- as.data.frame(occ)}

  #Get unique coordinates
  unique_xy <- occ[,c(long, lat, state_column)] %>% dplyr::distinct()
  # Spatialize
  dc <- terra::vect(unique_xy,
                    geom = c(x = long, y = lat),
                    crs = "+init=epsg:4326")

  #Looping with pbsapply
  if(verbose){
    message("Testing states...")}

  test_state <- pbapply::pbsapply(states_in, function(i){
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
  occ <- dplyr::left_join(occ, unique_xy, by = c(long, lat, state_column)) %>%
    dplyr::relocate(correct_state,
                    .after = dplyr::all_of(state_column))

  if(verbose){
    message(sum(!occ$correct_state, na.rm = TRUE), " records fall in wrong states")
  }

  if(try_to_fix){
    occ <- fix_states(occ, long, lat, state_column,
                         correct_state = "correct_state", distance, verbose)
  }

  return(occ)
}

# res2 <- check_states(occ = res,
#                         state_column = "state_suggested",
#                         try_to_fix = TRUE)


# occ <- res
# long = "decimalLongitude"
# lat = "decimalLatitude"
# state_column = "state_suggested"
# distance = 5
# verbose = T

