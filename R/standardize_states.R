standardize_states <- function(occ,
                               state_column = "stateProvince",
                               country_column = "country_suggested",
                               max_distance = 0.1,
                               lookup_na_state = FALSE,
                               long = NULL, lat = NULL,
                               return_dictionary = TRUE){
  # Get state dictionary
  ss <- RuHere::states_dictionary

  # Remove accents
  occ[[state_column]] <- remove_accent(occ[[state_column]])

  # Convert empty cells ("") in NA
  occ[[state_column]][occ[[state_column]] == ""] <- NA

  # Set states to lower
  occ[[state_column]][nchar(occ[[state_column]]) > 3 &
                          !is.na(occ[[state_column]])] <- tolower(
                            occ[[state_column]][nchar(occ[[state_column]]) > 3 &
                                                    !is.na(occ[[state_column]])])

  # Set codes to upper
  occ[[state_column]][nchar(occ[[state_column]]) <= 3 &
                          !is.na(occ[[state_column]])] <- toupper(
                            occ[[state_column]][nchar(occ[[state_column]]) <= 3 &
                                                    !is.na(occ[[state_column]])])

  # Check state names
  unique_states <- distinct(occ[, c(state_column, country_column)])

  ccn <- florabr:::match_names(species = na.omit(unique_states[[state_column]]),
                               species_to_match = ss$states_name$state_name,
                               max_distance = max_distance)
  colnames(ccn) <- c("state", "state_name", "Distance")
  # Join data
  ccn <- dplyr::left_join(na.omit(ccn), ss$states_name, by = "state_name") %>%
    dplyr::select(state, state_suggested, country) %>% dplyr::distinct()

  if(nrow(ccn) > 0){
    # Rename columns
    colnames(ccn) <- c(state_column, "state_suggested", country_column)} else {
      ccn <- NULL
    }

  # Check state codes
  colnames(ss$states_code) <- c("state_code", state_column, country_column)

  ccc <- ss$states_code %>% dplyr::filter(state_code %in% unique_states)

  ccc <- ss$states_code %>%
    dplyr::semi_join(unique_states,
              by = c(all_of(state_column), all_of(country_column)))

  if(nrow(ccc) > 0){
    # Rename columns
    colnames(ccc) <- c(state_column, "state_suggested", country_column)} else {
      ccc <- NULL
    }

  # Join information
  final_states <- dplyr::bind_rows(ccn, ccc)

  if(nrow(final_states) > 0){
    occ_final <- left_join(occ, final_states,
                           by = c(state_column, country_column)) %>%
      relocate(state_suggested, .after = state_column)} else {
        occ_final <- occ
      }

  # Fill NA?
  if(lookup_na_state){
    occ_final <- states_from_coords(occ_final, long, lat, from = "na_only",
                                     state_column = "state_suggested",
                                     output_column = "state_suggested",
                                     append_source = TRUE)
    occ_final$state_source[
      is.na(occ_final$state_source) &
        !is.na(occ_final[["state_suggested"]])] <- "metadata"
  }

  if(return_dictionary){
    states_out <- setdiff(unique_states[[state_column]],
                          final_states[[state_column]])
    states_out <- unique_states[unique_states[[state_column]] %in% states_out, ]
    states_out <- states_out[!is.na(states_out[[state_column]]), ]

    if(nrow(states_out) > 0) {
      states_out$state_suggested <- NA } else {
        states_out <- NULL
        }
    r <- dplyr::bind_rows(final_states,
                          states_out)
    return(list("occ" = occ_final,
                "report" = r))
  } else {#End of return_dictionary
    return(occ_final)
  }
} #End of function

# # For test
# occ_gbif <- RuHere::occ_gbif
# occ_splink <- RuHere::occ_splink
# occ_bien <- RuHere::occ_bien
# # Format columns
# occ_gbif <- format_columns(occ_gbif, metadata = "gbif")
# occ_splink <- format_columns(occ_splink, metadata = "specieslink")
# occ_bien <- format_columns(occ_bien, metadata = "bien")
# # Merge data
# all_occ <- bind_rows(occ_gbif, occ_splink, occ_bien)

# res <- standardize_states(all_occ)
# View(res$occ)
# View(res$report)
# res <- standardize_states(all_occ, return_dictionary = F)
# View(res$occ)
# View(res$report)

