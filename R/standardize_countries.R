standardize_countries <- function(occ,
                                  country_column = "country",
                                  max_distance = 0.1,
                                  return_report = TRUE){
  # Get country dictionary
  cd <- RuHere::country_dictionary

  # Remove accents
  occ[[country_column]] <- remove_accent(occ[[country_column]])


  # Set countries to lower
  occ[[country_column]][nchar(occ[[country_column]]) > 3 &
                          !is.na(occ[[country_column]])] <- tolower(
    occ[[country_column]][nchar(occ[[country_column]]) > 3 &
                            !is.na(occ[[country_column]])])

  # Set codes to upper
  occ[[country_column]][nchar(occ[[country_column]]) <= 3 &
                          !is.na(occ[[country_column]])] <- toupper(
                            occ[[country_column]][nchar(occ[[country_column]]) <= 3 &
                                                    !is.na(occ[[country_column]])])

  # Check country names
  unique_countries <- na.omit(unique(occ[[country_column]]))

  ccn <- florabr:::match_names(species = unique_countries,
                               species_to_match = cd$country_name$country_name,
                               max_distance = max_distance)
  colnames(ccn) <- c("country", "country_name", "Distance")
  # Join data
  ccn <- dplyr::left_join(na.omit(ccn), cd$country_name, by = "country_name") %>%
    dplyr::select(country, country_suggested) %>% dplyr::distinct()

  if(nrow(ccn) > 0){
    # Rename columns
    colnames(ccn) <- c(country_column, "country_suggested")} else {
      ccn <- NULL
    }

  # Check country codes
  ccc <- cd$country_code %>% dplyr::filter(country_code %in% unique_countries)

  if(nrow(ccc) > 0){
    # Rename columns
    colnames(ccc) <- c(country_column, "country_suggested")} else {
      ccc <- NULL
    }

  # Join information
  final_countries <- dplyr::bind_rows(ccn, ccc)

  if(nrow(final_countries) > 0){
  occ_final <- left_join(occ, final_countries, by = country_column) %>%
    relocate(country_suggested, .after = country_column)} else {
      occ_final <- occ
    }

  if(return_report){
    countries_out <- setdiff(unique_countries, final_countries$country)
    if(length(countries_out) > 0) {
      countries_out <- data.frame("country" = countries_out,
                                  "country_suggested" = NA) } else {
        countries_out <- NULL
      }
      r <- dplyr::bind_rows(final_countries,
                     countries_out)
      return(list("occ" = occ_final,
                  "report" = r))
  } else {#End of return_report
    return(occ_final)
  }
} #End of function

# # For test
# res <- standardize_countries(all_occ)
# View(res$occ)
# View(res$report)
# res <- standardize_countries(all_occ, return_report = F)
# View(res$occ)
# View(res$report)

