remove_invalid_coordinates <- function(occ, 
                                       long = "decimalLongitude", 
                                       lat = "decimalLatitude",
                                       return_invalid = TRUE){
  # Force numeric
  if(!inherits(occ[[long]], "numeric"))
    occ[[long]] <- as.numeric(occ[[long]])
  if(!inherits(occ[[lat]], "numeric"))
    occ[[lat]] <- as.numeric(occ[[lat]])
  
  # Identify invalids
  invalid <- which(occ[[long]] > 180 | occ[[long]] < -180 |
                     occ[[lat]] > 90 | occ[[lat]] < -90 |
                     is.na(occ[[long]]) | is.na(occ[[lat]]))
  
  if(return_invalid){
    return(list(valid = occ[!invalid,],
                invalid = occ[invalid,]))
  } else {
    return(occ[!invalid,])
  }
  
}