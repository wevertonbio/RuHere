standardize_countries <- function(occ,
                                  country_column = "country",
                                  max_distance = 0.1,
                                  user_dictionary = NULL,
                                  lookup_na_country = FALSE,
                                  long = NULL, lat = NULL,
                                  return_dictionary = TRUE){
  # Get country dictionary
  cd <- RuHere::country_dictionary

  # Bind user dictionary
  if(!is.null(user_dictionary)){
    cd$country_name <- rbind(cd$country_name, user_dictionary)
  }


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
  occ_final <- left_join(occ, final_countries, by = country_column) } else {
      occ_final <- occ
      occ_final$country_suggested <- NA
  }

  # Relocate columns
  occ_final <- occ_final %>%
    relocate(country_suggested, .after = dplyr::all_of(country_column))

  # Fill NA?
  if(lookup_na_country){
    occ_final <- country_from_coords(occ_final, long, lat,
                                      country_column = "country_suggested",
                                      output_column = "country_suggested",
                                      from = "na_only", append_source = TRUE)
    occ_final$country_source[
      is.na(occ_final$country_source) &
        !is.na(occ_final[["country_suggested"]])] <- "metadata"
  }


  # Return dictionary?
  if(return_dictionary){
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
  } else {#End of return_dictionary
    return(occ_final)
  }
} #End of function

# # # For test
# occ_gbif <- RuHere::occ_gbif
# occ_splink <- RuHere::occ_splink
# occ_bien <- RuHere::occ_bien
# # Format columns
# occ_gbif <- format_columns(occ_gbif, metadata = "gbif")
# occ_splink <- format_columns(occ_splink, metadata = "specieslink")
# occ_bien <- format_columns(occ_bien, metadata = "bien")
# # Merge data
# all_occ <- bind_rows(occ_gbif, occ_splink, occ_bien)
# res <- standardize_countries(all_occ)
# View(res$occ)
# View(res$report)
# res <- standardize_countries(all_occ, return_dictionary = F)
# View(res$occ)
# View(res$report)
# res <- standardize_countries(all_occ, lookup_na_country = TRUE,
#                              long = "decimalLongitude", lat = "decimalLatitude",
#                              return_dictionary = TRUE)
# res$occ %>% select(dplyr::starts_with("countr")) %>% View()
