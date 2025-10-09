country_from_coords <- function(occ,
                                long = "decimalLongitude",
                                lat = "decimalLatitude",
                                country_column = "country",
                                from = "all"){ #or na_only
  # Get map of world
  w <- vect(system.file("extdata/world.gpkg", package = "RuHere"))

  # Extract
  if(from == "all"){
    occ$country_xy <- terra::extract(w, occ[, c(long, lat)])[[2]]
    }

  if(from == "na_only"){
    na_country <- which(is.na(occ[[country_column]]))
    occ$country_xy[na_country] <- terra::extract(w,
                                                 occ[na_country,
                                                     c(long, lat)])[[2]]
  }

  #Reorder columns
  occ <- occ %>% dplyr::relocate(country_xy,
                                 .after = dplyr::all_of(country_column))

}
