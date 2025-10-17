states_from_coords <- function(occ,
                               long = "decimalLongitude",
                               lat = "decimalLatitude",
                               from = "all", #or na_only
                               state_column = "stateProvince",
                               output_column = "state_xy",
                               append_source = FALSE){
  # Get map of states
  s <- terra::vect(system.file("extdata/states.gpkg", package = "RuHere"))

  # Extract
  if(from == "all"){
    occ[[output_column]] <- NA
    occ[[output_column]] <- terra::extract(s, occ[, c(long, lat)])[[2]]
  }

  if(from == "na_only"){
    na_state <- which(is.na(occ[[state_column]]) | occ[[state_column]] == "")
    occ[na_state, output_column] <- terra::extract(s,
                                                   occ[na_state,
                                                       c(long, lat)])[[2]]
  }

  #Reorder columns, if necessary
  if(state_column != output_column){
    occ <- occ %>% dplyr::relocate(dplyr::all_of(output_column),
                                   .after = dplyr::all_of(state_column))}

  if(append_source && state_column == output_column){
    occ <- occ %>% dplyr::mutate(state_source = NA,
                                 .after = dplyr::all_of(output_column))
    to_append <- intersect(na_state, which(!is.na(occ[[output_column]])))
    occ$state_source[to_append] <- "coords"
  }

  return(occ)
}
