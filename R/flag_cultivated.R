flag_cultivated <- function(occ,
                            columns = c("occurrenceRemarks", "habitat",
                                        "locality"),
                            cultivated_terms = NULL,
                            not_cultivated_terms = NULL){

  if(!is.null(cultivated_terms)){
    cultivated <- c(RuHere::cultivated$cultivated, cultivated_terms)
  } else {
    cultivated <- RuHere::cultivated$cultivated
  }

  if(!is.null(not_cultivated_terms)){
    not_cultivated <- c(RuHere::cultivated$not_cultivated, not_cultivated_terms)
  } else {
    not_cultivated <- RuHere::cultivated$not_cultivated
  }

  # Combine in single vector
  cultivated <- paste(cultivated, collapse = "|")

  # First, check cultivated...
  cultivated_index <- sapply(columns, function(i){
    which(stringi::stri_detect_regex(occ[[i]],
                               pattern = cultivated))
  }, USE.NAMES = FALSE)
  cultivated_index <- unlist(cultivated_index)

  # If there are cultivated, check not cultivated
  if(length(cultivated_index) > 0){
    not_cultivated <- paste(not_cultivated, collapse = "|")
    not_cultivated_index <- sapply(columns, function(i){
      which(stringi::stri_detect_regex(occ[cultivated_index, i],
                                       pattern = not_cultivated))
    }, USE.NAMES = FALSE)
    not_cultivated_index <- unlist(not_cultivated_index)
    if(length(not_cultivated_index) > 0){
      cultivated_index <- cultivated_index[-not_cultivated_index]
    }
  }

  # Create column to save flags
  occ$cultivated_flag <- TRUE
  # Flag if necessary
  if(length(cultivated_index) > 0){
    occ$cultivated_flag[cultivated_index] <- FALSE
  }

  return(occ)
}


# occ <- RuHere::occurrences
# occ2 <- flag_cultivated(occ = occurrences)
