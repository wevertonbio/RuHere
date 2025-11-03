#' Flag fossil
#'
#' @param occ (data.frame)
#' @param columns (character)
#' @param fossil_terms (character)
#'
#' @returns A dataframe
#' @export
#' @importFrom stringi stri_detect_regex
flag_fossil <- function(occ,
                        columns = c("basisOfRecord", "occurrenceRemarks"),
                        fossil_terms = NULL){

  if(!is.null(fossil_terms)){
    fossil <- c("fossil", fossil_terms)
  } else {
    fossil <- "fossil"
  }

  # Combine in single vector
  fossil <- paste(fossil, collapse = "|")

  # First, check fossil...
  fossil_index <- sapply(columns, function(i){
    which(stringi::stri_detect_regex(occ[[i]],
                                     pattern = fossil))
  }, USE.NAMES = FALSE)
  fossil_index <- unlist(fossil_index)

  # Create column to save flags
  occ$fossil_flag <- TRUE
  # Flag if necessary
  if(length(fossil_index) > 0){
    occ$fossil_flag[fossil_index] <- FALSE
  }

  return(occ)
}


# occ <- RuHere::occurrences
# occ2 <- flag_fossil(occ = occurrences)
