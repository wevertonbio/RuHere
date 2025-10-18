#### Script for test data ####
devtools::load_all()
library(data.table)
library(dplyr)

#### Format columns ####
# Import data
# SpeciesLink
occ <- RuHere::occ_splink
occ_splink <- format_columns(occ = occ, metadata = "specieslink")
# # GBIF
occ <- RuHere::occ_gbif
occ_gbif <- format_columns(occ = occ, metadata = "gbif")
# BIEN
occ <- RuHere::occ_bien
occ_bien <- format_columns(occ = occ, metadata = "bien")
# Merge data with bind_rows
all_occ <- bind_rows(occ_splink, occ_gbif, occ_bien)

#### Bind ocurrences ####
all_occ <- bind_here(occ_splink, occ_gbif, occ_bien, fake_data)


#### Standardize countries ####
res <- standardize_countries(all_occ,
                             lookup_na_country = TRUE,
                             long = "decimalLongitude", lat = "decimalLatitude",
                             return_dictionary = F)

#### Standardize States ####
res <- standardize_states(res, return_dictionary = F,
                          lookup_na_state = TRUE,
                          long = "decimalLongitude", lat = "decimalLatitude")

#### Check and fix coutries####
res_countries <- check_countries(occ = res,
                                 country_column = "country_suggested",
                                 try_to_fix = TRUE)
res_countries %>% select(country_suggested, country_issues) %>% View()

#### Check and fix states ####
res_states <- check_states(occ = res,
                           state_column = "state_suggested",
                           try_to_fix = TRUE)
