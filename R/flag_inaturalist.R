#' Title
#'
#' @param occ
#' @param columns
#' @param research_grade
#'
#' @returns
#' @export
#'
#' @examples
flag_inaturalist <- function(occ,
                             columns = "datasetName",
                             research_grade = FALSE){
  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # First, check inaturalist
  inat_index <- lapply(columns, function(i){
    which(stringi::stri_detect_regex(occ[[i]],
                                     pattern = "iNaturalist"))
  })
  inat_index <- unlist(inat_index)

  # If do not flash research-grade...
  if(length(inat_index) > 0 & !research_grade){
    rg_term <- c("research-grade|researchgrade|research_grade|research grade")
    rg_index <- lapply(columns, function(i){
      which(stringi::stri_detect_regex(occ[inat_index, i],
                                       pattern = rg_term))
    })
    rg_index <- unlist(rg_index)
    if(length(rg_index) > 0){
      inat_index <- inat_index[-rg_index]
    }
  }

    # Create column to save flags
    occ$inaturalist_flag <- TRUE

    # Flag if necessary
    if(length(inat_index) > 0){
      occ$inaturalist_flag[inat_index] <- FALSE
    }

    return(occ)
  }


# occ2 <- flag_inaturalist(occ = occurrences, research_grade = FALSE)
# sum(occ2$inaturalist_flag)
# occ2 <- flag_inaturalist(occ = occurrences, research_grade = TRUE)
# sum(!occ2$inaturalist_flag)
