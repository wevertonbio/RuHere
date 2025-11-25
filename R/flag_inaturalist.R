#' Flag occurrence records sourced from iNaturalist
#'
#' @description
#' This function identifies and flags occurrence records sourced from
#' iNaturalist. It can flag all iNaturalist records or only those that do not
#' have Research Grade status.
#'
#' @param occ (data.frame) a data frame containing the occurrence records to be
#' examined, preferably standardized using `format_columns()`. Must contain the
#' columns specified in `columns`.
#' @param columns (character) column name in `occ` where the
#' function will search for the term "iNaturalist". Default is "datasetName".
#' @param research_grade (logical) whether to flag *all* records from
#' iNaturalist, including those with Research Grade status. Default is `FALSE`,
#' meaning that only iNaturalist records **without** Research Grade will be
#' flagged.
#'
#' @details
#' According to [iNaturalist](https://help.inaturalist.org/en/support/solutions/articles/151000169936-what-is-the-data-quality-assessment-and-how-do-observations-qualify-to-become-research-grade-), Observations become Research Grade when:
#' - the iNaturalist community agrees on species-level ID or lower, i.e. when
#' more than 2/3 of identifiers agree on a taxon;
#' - the community taxon and the observation taxon agree;
#' - or the community agrees on an ID between family and species and votes that
#' the community taxon is as good as it can be.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame augmented with
#' a new column named \code{inaturalist_flag}. Flagged records receive
#' \code{FALSE}, while all other records receive \code{TRUE}.
#'
#' @export
#'
#' @importFrom stringi stri_detect_regex
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Flag only iNaturalist records without Research Grade
#' occ_inat <- flag_inaturalist(occ = occurrences, research_grade = FALSE)
#' table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)
#' # Flag all iNaturalist records (including Research Grade)
#' occ_inat <- flag_inaturalist(occ = occurrences, research_grade = TRUE)
#' table(occ_inat$inaturalist_flag) # Number of records flagged (FALSE)
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
