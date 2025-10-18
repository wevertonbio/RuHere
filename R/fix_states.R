fix_states <- function(occ,
                       long = "decimalLongitude",
                       lat = "decimalLatitude",
                       state_column,
                       correct_state = "correct_state",
                       distance = 5,
                       verbose = TRUE){
  #Convert to data.frame if necessary
  if(inherits(occ, "data.table")){
    occ <- as.data.frame(occ)}

  #Subset occ
  occ_correct <- occ %>%
    filter(.data[[correct_state]] | is.na(.data[[correct_state]]))
  occ_incorrect <- occ %>% filter(!.data[[correct_state]])

  #Create columns to identify problems
  occ_correct[["state_issues"]] <- "correct"
  occ_incorrect[["state_issues"]] <- "incorrect"

  #Get shapefile with states
  state_shp <- terra::vect(system.file("extdata/states.gpkg",
                                       package = "RuHere"))

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
  test_1 <- pbapply::pbsapply(states_1, function(i){
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
  occ_correct <- dplyr::bind_rows(occ_correct,
                                  occ_incorrect %>%
                                    filter(state_issues != "incorrect"))
  occ_incorrect <- occ_incorrect %>%
    dplyr::filter(state_issues == "incorrect")

  ####2 - Inverted signal of latitude ####
  if(nrow(occ_incorrect) > 0) {
    d2 <- occ_incorrect[,c(long,lat, state_column)]
    states_2 <- intersect(unique(d2[,state_column]), all_states)
    d2[, lat] <- - d2[,lat]
    d2 <- terra::vect(d2,
                      geom = c(x = long, y = lat),
                      crs = "+init=epsg:4326")
    message("Task 2 of 7: testing if latitude is inverted")
    test_2 <- pbapply::pbsapply(states_2, function(i){
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
    occ_correct <- dplyr::bind_rows(occ_correct,
                                    occ_incorrect %>%
                                      filter(state_issues != "incorrect"))
    occ_incorrect <- occ_incorrect %>%
      dplyr::filter(state_issues == "incorrect")
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
    test_3 <- pbapply::pbsapply(states_3, function(i){
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
    occ_correct <- dplyr::bind_rows(occ_correct,
                                    occ_incorrect %>%
                                      filter(state_issues != "incorrect"))
    occ_incorrect <- occ_incorrect %>%
      dplyr::filter(state_issues == "incorrect")
  }

  ####4 - Swap longitude and latitude ####
  if(nrow(occ_incorrect) > 0) {
    d4 <- occ_incorrect[,c(long,lat, state_column)]
    states_4 <- intersect(unique(d4[,state_column]), all_states)
    d4 <- terra::vect(d4,
                      geom = c(x = lat, y = long),
                      crs = "+init=epsg:4326")
    message("Task 4 of 7: testing if longitude and latitude are swapped")
    test_4 <- pbapply::pbsapply(states_4, function(i){
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
    occ_correct <- dplyr::bind_rows(occ_correct,
                                    occ_incorrect %>%
                                      filter(state_issues != "incorrect"))
    occ_incorrect <- occ_incorrect %>%
      dplyr::filter(state_issues == "incorrect")
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
    test_5 <- pbapply::pbsapply(states_5, function(i){
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
    occ_correct <- dplyr::bind_rows(occ_correct,
                                    occ_incorrect %>%
                                      filter(state_issues != "incorrect"))
    occ_incorrect <- occ_incorrect %>%
      dplyr::filter(state_issues == "incorrect")
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
    test_6 <- pbapply::pbsapply(states_6, function(i){
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
    occ_correct <- dplyr::bind_rows(occ_correct,
                                    occ_incorrect %>%
                                      filter(state_issues != "incorrect"))
    occ_incorrect <- occ_incorrect %>%
      dplyr::filter(state_issues == "incorrect")
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
    test_7 <- pbapply::pbsapply(states_7, function(i){
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
    occ_correct <- dplyr::bind_rows(occ_correct,
                                    occ_incorrect %>%
                                      filter(state_issues != "incorrect"))
    occ_incorrect <- occ_incorrect %>%
      dplyr::filter(state_issues == "incorrect")
  }

  #Final occset
  if(nrow(occ_incorrect) == 0) {
    occ <- occ_correct
  } else {
    occ <- bind_rows(occ_correct, occ_incorrect)
  }

  # Reorder columns
  occ <- occ %>% dplyr::relocate(state_issues,
                                 .after = dplyr::all_of(correct_state))

  return(occ)
}
