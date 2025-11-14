flag_year <- function(occ,
                      year_column = "year",
                      lower_limit = NULL,
                      upper_limit = NULL,
                      flag_NA = FALSE,
                      verbose = TRUE){

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # Create column to flag
  occ$year_flag <- TRUE
  if(!is.null(lower_limit)){
    occ$year_flag[occ[[year_column]] < lower_limit &
                    !is.na(occ[[year_column]])] <- FALSE
  }

  if(!is.null(upper_limit)){
    occ$year_flag[occ[[year_column]] > upper_limit &
                    !is.na(occ[[year_column]])] <- FALSE
  }

  if(flag_NA){
    occ$year_flag[is.na(occ[[year_column]])] <- FALSE
  }

  return(occ)
}

# # Example
# occ <- RuHere::occurrences
# range(occ$year, na.rm = TRUE)
# year_column = "year"
# lower_limit = 1980
# upper_limit = 2024
# flag_NA = FALSE
# occ_year <- flag_year(occ = occ, lower_limit = 1980, upper_limit = 2020, flag_NA = TRUE)
# View(occ_year[, c("year", "year_flag")])
