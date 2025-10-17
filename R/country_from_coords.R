country_from_coords <- function(occ,
                                long = "decimalLongitude",
                                lat = "decimalLatitude",
                                from = "all",#or na_only
                                country_column = NULL,
                                output_column = "country_xy",
                                append_source = FALSE){
  # Get map of world
  w <- terra::vect(system.file("extdata/world.gpkg", package = "RuHere"))

  # Extract
  if(from == "all"){
    occ[[output_column]] <- NA
    occ[[output_column]] <- terra::extract(w, occ[, c(long, lat)])[[2]]
    }

  if(from == "na_only"){
    na_country <- which(is.na(occ[[country_column]]))
    occ[na_country, output_column] <- terra::extract(w,
                                                 occ[na_country,
                                                     c(long, lat)])[[2]]
  }

  #Reorder columns, if necessary
  if(!is.null(country_column) && country_column != output_column){
    occ <- occ %>% dplyr::relocate(dplyr::all_of(output_column),
                                   .after = dplyr::all_of(country_column))}

  if(append_source && country_column == output_column){
    occ <- occ %>% dplyr::mutate(country_source = NA,
                                 .after = dplyr::all_of(output_column))
    to_append <- intersect(na_country, which(!is.na(occ[[output_column]])))
    occ$country_source[to_append] <- "coords"
  }

  return(occ)

}
