spatialize <- function(occ, long = "decimalLongitude", lat = "decimalLatitude",
                       crs = "epsg:4326", force_numeric = TRUE){
  if(force_numeric){
    if(!inherits(occ[[long]], "numeric"))
      occ[[long]] <- as.numeric(occ[[long]])
    if(!inherits(occ[[lat]], "numeric"))
      occ[[lat]] <- as.numeric(occ[[lat]])
  }

  # Remove NAs
  occ <- occ[!is.na(occ[[long]]) & !is.na(occ[[lat]]), ]
  vect(occ, geom = c(x = long, y = lat), crs = crs)
}
