# #### FORMAT COLUMNS OF ATLANTICR ####
# # https://mauriciovancine.github.io/project/atlanticr/
#
# #Load packages
# library(RuHere)
# library(data.table)
# library(dplyr)
# library(atlanticr)
#
# #### ATLANTIC EPIPHYTES ####
# ae <- atlanticr::atlantic_epiphytes
# #Split dataframe in EPIPHYTE and FOROPYHYTE
# ae_epi <- ae %>% mutate(species = florabr::get_binomial(epiphyte_species))
# ae_for <- ae %>% mutate(species = florabr::get_binomial(foropyhyte_species))
# ae <- bind_rows(ae_epi, ae_for)
#
# #Fix some columns
# ae$municipality <- ae$municipality %>% florabr:::firstup()
# ae$state <- ae$state %>% florabr:::firstup()
# ae$precision <- as.numeric(ae$precision)
# ae$occurrenceID <- 1:nrow(ae)
# ae$basisOfRecord <- "datapaper"
# #Create metadata of columns
# ae_metadata <- data.frame(scientificName = "species",
#                           occurrenceID = "occurrenceID",
#                           collectionCode = NA,
#                           catalogNumber = NA,
#                           decimalLongitude = "longitude_x",
#                           decimalLatitude = "latitude_y",
#                           coordinateUncertaintyInMeters = "precision",
#                           elevation = "altitude",
#                           country = "country",
#                           stateProvince = "state",
#                           municipality = "municipality",
#                           locality = "regional_name_of_study_site",
#                           year = "year_start",
#                           habitat = "habitat",
#                           occurrenceRemarks = NA,
#                           eventDate = NA,
#                           recordedBy = NA,
#                           identifiedBy = NA,
#                           basisOfRecord = "basisOfRecord",
#                           datasetName = "dataset_acronym",
#                           datasetKey = NA,
#                           key = NA)
# #Fix columns
# ae_fixed <- format_columns(occ = ae, metadata = ae_metadata,
#                            data_source = "atlantic_epiphyte",
#                            binomial_from = "species")
# # Add kingdom
# ae_fixed <- ae_fixed |>
#   mutate(kingdom = "Plantae", .before = 1)
#
# #Save
# fwrite(ae_fixed, "../RuHere_paper/AtlanticR/atlantic_epiphyte.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### POLLINATION ####
# dp_pollination <- atlantic_pollination
# head(dp_pollination)
#
# #Create basisOfRecord
# dp_pollination$basisOfRecord <- "datapaper"
#
# # Split plant and animal
# plant_pollination <- dp_pollination |>
#   mutate(scientificName = paste(dp_pollination$genera_plant,
#                                 dp_pollination$sp_plant,
#                                 sep = " "), .before = 1)
# animal_pollination <- dp_pollination |>
#   mutate(scientificName = paste(dp_pollination$genera_floralvisitor,
#                                 dp_pollination$sp_floralvisitor,
#                                 sep = " "), .before = 1)
# #Create metadata of columns
# dp_pollination_metadata <- data.frame(scientificName = "scientificName",
#                                       occurrenceID = "id",
#                                       collectionCode = NA,
#                                       catalogNumber = NA,
#                                       decimalLongitude = "longitude",
#                                       decimalLatitude = "latitude",
#                                       coordinateUncertaintyInMeters = NA,
#                                       elevation = NA,
#                                       country = NA,
#                                       stateProvince = NA,
#                                       municipality = NA,
#                                       locality = NA,
#                                       year = NA,
#                                       eventDate = NA,
#                                       recordedBy = NA,
#                                       identifiedBy = NA,
#                                       basisOfRecord = "basisOfRecord",
#                                       datasetName = NA,
#                                       datasetKey = NA,
#                                       key = NA,
#                                       occurrenceRemarks = NA, habitat = NA)
# #Fix columns
# plant_pollination_fixed <- format_columns(occ = plant_pollination,
#                                           metadata = dp_pollination_metadata,
#                                           data_source = "atlantic_pollination",
#                                           binomial_from = "scientificName")
# # Add kingdom
# plant_pollination_fixed <- plant_pollination_fixed |>
#   mutate(kingdom = "Plantae", .before = 1)
#
# #Fix columns
# animal_pollination_fixed <- format_columns(occ = animal_pollination,
#                                           metadata = dp_pollination_metadata,
#                                           data_source = "atlantic_pollination",
#                                           binomial_from = "scientificName")
# # Add kingdom
# animal_pollination_fixed <- plant_pollination_fixed |>
#   mutate(kingdom = "Animalia", .before = 1)
#
# # Merge
# pollination <- rbind(plant_pollination_fixed,
#                      animal_pollination_fixed)
# # Save
# #Save
# fwrite(pollination, "../RuHere_paper/AtlanticR/atlantic_pollination.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### PLANT INVERTEBRATE INTERACTION ####
# dp_plant_inv <- atlantic_flower_invertebrate_interactions
# head(dp_plant_inv)
# #Create basisOfRecord
# dp_plant_inv$basisOfRecord <- "datapaper"
#
# # Split plant and animal
# plant_inv <- dp_plant_inv |>
#   mutate(scientificName = plant_species, .before = 1)
# plant_inv$identifiedBy <- plant_inv$plant_identification_responsability
#
# animal_inv <- dp_plant_inv |>
#   mutate(scientificName = invertebrate_species, .before = 1)
# animal_inv$identifiedBy <- animal_inv$invertebrate_identification_responsability
# #Create metadata of columns
# dp_plant_inv_metadata <- data.frame(scientificName = "scientificName",
#                                     occurrenceID = "record_id",
#                                     collectionCode = NA,
#                                     catalogNumber = NA,
#                                     decimalLongitude = "longitude_x",
#                                     decimalLatitude = "latitude_y",
#                                     coordinateUncertaintyInMeters = NA,
#                                     elevation = NA,
#                                     country = "country",
#                                     stateProvince = "state",
#                                     municipality = "municipality",
#                                     locality = NA,
#                                     year = "campain_year_finish",
#                                     eventDate = NA,
#                                     recordedBy = NA,
#                                     identifiedBy = "identifiedBy",
#                                     basisOfRecord = "basisOfRecord",
#                                     datasetName = NA,
#                                     datasetKey = NA,
#                                     key = NA,
#                                     occurrenceRemarks = NA, habitat = NA)
# # Fix columns
# plant_inv_fixed <- format_columns(occ = plant_inv,
#                                   metadata = dp_plant_inv_metadata,
#                                   data_source = "atlantic_flower_invertebrate_interactions",
#                                   binomial_from = "scientificName")
# # Add kingdom
# plant_inv_fixed <- plant_inv_fixed |>
#   mutate(kingdom = "Plantae", .before = 1)
#
# #Fix columns
# animal_inv_fixed <- format_columns(occ = animal_inv,
#                                    metadata = dp_plant_inv_metadata,
#                                    data_source = "atlantic_flower_invertebrate_interactions",
#                                    binomial_from = "scientificName")
# # Add kingdom
# animal_inv_fixed <- animal_inv_fixed |>
#   mutate(kingdom = "Animalia", .before = 1)
#
# # Merge
# plant_inv_final <- rbind(plant_inv_fixed,
#                          animal_inv_fixed)
# # Remove records without species names
# plant_inv_final2 <- plant_inv_final |> filter(!is.na(species))
#
# # Save
# fwrite(plant_inv_final2,
#        "../RuHere_paper/AtlanticR/atlantic_flower_invertebrate_interactions.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### MAMMALS ####
# dp_mammals <- atlantic_mammals
# head(dp_mammals)
# #Create occurrence ID
# dp_mammals$ocurrenceID <- paste(dp_mammals$ID, 1:nrow(dp_mammals), sep = "_")
# #Create basisOfRecord
# dp_mammals$basisOfRecord <- "datapaper"
# #Create metadata of columns
# dp_mammals_metadata <- data.frame(scientificName = "actual_species_name",
#                                   occurrenceID = "ocurrenceID",
#                                   collectionCode = NA,
#                                   catalogNumber = NA,
#                                   decimalLongitude = "longitude",
#                                   decimalLatitude = "latitude",
#                                   coordinateUncertaintyInMeters = NA,
#                                   elevation = "altitude",
#                                   country = "country",
#                                   stateProvince = "state",
#                                   municipality = "municipality",
#                                   locality = "study_location",
#                                   year = "year_finish",
#                                   eventDate = NA,
#                                   recordedBy = NA,
#                                   identifiedBy = NA,
#                                   basisOfRecord = "basisOfRecord",
#                                   datasetName = NA,
#                                   datasetKey = NA,
#                                   key = NA, occurrenceRemarks = NA,
#                                   habitat = NA)
# #Fix columns
# dp_mammals_fixed <- format_columns(occ = dp_mammals,
#                                    binomial_from = "actual_species_name",
#                                    metadata = dp_mammals_metadata,
#                                    data_source = "atlantic_mammals")
# # Add kingdom
# dp_mammals_fixed <- dp_mammals_fixed |>
#   mutate(kingdom = "Animalia", .before = 1)
#
# #Save
# fwrite(dp_mammals_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_mammals.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### Primates ####
# dp_primates <- atlantic_primates
# head(dp_primates)
# #Create basisOfRecord
# dp_primates$basisOfRecord <- "datapaper"
#
# #Create metadata of columns
# dp_primates_metadata <- data.frame(scientificName = "species",
#                                    occurrenceID = "id",
#                                    collectionCode = NA,
#                                    catalogNumber = NA,
#                                    decimalLongitude = "longitude_x",
#                                    decimalLatitude = "latitude_y",
#                                    coordinateUncertaintyInMeters = NA,
#                                    elevation = "altitude",
#                                    country = "country",
#                                    stateProvince = "state",
#                                    municipality = "municipality",
#                                    locality = "site",
#                                    year = "col_strt_yr",
#                                    eventDate = NA,
#                                    recordedBy = NA,
#                                    identifiedBy = NA,
#                                    basisOfRecord = "basisOfRecord",
#                                    datasetName = NA,
#                                    datasetKey = NA,
#                                    key = NA, occurrenceRemarks = NA,
#                                    habitat = NA)
# #Fix columns
# dp_primates_fixed <- format_columns(occ = dp_primates, binomial_from = "species",
#                                     metadata = dp_primates_metadata,
#                                     data_source = "atlantic_primates")
# # Add kingdom
# dp_primates_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(dp_primates_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_primates.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### Amphibians ####
# dp_amphibians_sites <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_AMPHIBIANS_sites.csv", encoding = "Latin-1")
# dp_amphibians_species <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_AMPHIBIANS_species.csv")
# #Merge data
# dp_amphibians <- left_join(dp_amphibians_species, dp_amphibians_sites, by = "id")
# head(dp_amphibians)
# #Create occurrence id
# dp_amphibians$occurrenceID <- 1:nrow(dp_amphibians)
# #Create basisOfRecord
# dp_amphibians$basisOfRecord <- "datapaper"
# #Create metadata of columns
# dp_amphibians_metadata <- data.frame(scientificName = "valid_name",
#                                      occurrenceID = "occurrenceID",
#                                      collectionCode = NA,
#                                      catalogNumber = NA,
#                                      decimalLongitude = "longitude",
#                                      decimalLatitude = "latitude",
#                                      coordinateUncertaintyInMeters = NA,
#                                      elevation = "altitude",
#                                      country = "country",
#                                      stateProvince = "state",
#                                      municipality = "municipality",
#                                      locality = "site",
#                                      year = "year_finish",
#                                      eventDate = NA,
#                                      recordedBy = NA,
#                                      identifiedBy = NA,
#                                      basisOfRecord = "basisOfRecord",
#                                      datasetName = NA,
#                                      datasetKey = NA,
#                                      key = NA,
#                                      occurrenceRemarks = NA,
#                                      habitat = NA)
# #Fix columns
# dp_amphibians_fixed <- format_columns(occ = dp_amphibians,
#                                       binomial_from = "valid_name",
#                                       metadata = dp_amphibians_metadata,
#                                       data_source = "atlantic_amphibians")
# # Add kingdom
# dp_amphibians_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(dp_amphibians_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_amphibians.gz",
#        compress = "gzip", row.names = FALSE)
#
#
# #Filter records without coordinates
# dp_amphibians_fixed <- dp_amphibians_fixed %>%
#   filter(!is.na(decimalLatitude), !is.na(decimalLongitude))
# #Now, subset species
# dp_amphibians_in <- dp_amphibians_fixed %>% filter(scientificName %in% spp$spp)
# #Get species out
# dp_amphibians_out <- dp_amphibians_fixed %>% filter(!(scientificName %in% spp$spp)) %>%
#   select(scientificName) %>% distinct()
#
# #Check names
# dp_amphibians_checked <- check_names_fauna(species = dp_amphibians_out$scientificName,
#                                            species_to_match = spp$spp,
#                                            parallel = TRUE, ncores = 10)
#
#
# #Get only suggestions if maximum distance is 2
# dp_amphibians_checked_01 <- dp_amphibians_checked %>% filter(Distance <= 2, matches == "single")
# #Adde other species with higher distances
# # dp_amphibians_checked_01 <- dp_amphibians_checked %>%
# #   filter(Suggested_name == "Artibeus (Artibeus) lituratus") %>%
# #   bind_rows(dp_amphibians_checked_01)
#
# #Subset species and change names
# dp_amphibians_in2 <- dp_amphibians_fixed %>% filter(scientificName %in% dp_amphibians_checked_01$input_name)
# dp_amphibians_in2 <- dp_amphibians_in2 %>% left_join(., dp_amphibians_checked_01[,c("input_name", "Suggested_name")],
#                                                      join_by(scientificName == input_name)) %>%
#   mutate(scientificName = Suggested_name) %>%
#   select(-Suggested_name)
#
# #Join records
# dp_amphibians_spp <- bind_rows(dp_amphibians_in2, dp_amphibians_in)
# #Create columns with species
# dp_amphibians_spp <- left_join(dp_amphibians_spp, spp,
#                                join_by(scientificName == spp)) %>%
#   relocate(species)
# any(is.na(dp_amphibians_spp$species))
# #Save data
# dir.create("Data/Animals/Occurrence_data/Atlantic_amphibians", recursive = TRUE)
# fwrite(dp_amphibians_spp, "Data/Animals/Occurrence_data/Atlantic_amphibians/Occurrences_dwc.gz",
#        row.names = FALSE, compress = "gzip")
#
#
# #### Bats ####
# dp_bats_sites <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_BATS_Study_site.csv", encoding = "Latin-1")
# dp_bats_species <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_BATS_Capture.csv")
# #Merge data
# dp_bats <- left_join(dp_bats_species, dp_bats_sites, by = "ID")
# head(dp_bats)
# #Create occurrence id
# dp_bats$occurrenceID <- 1:nrow(dp_bats)
# #Fix species names
# dp_bats$Species <- gsub("\\.", " ", dp_bats$Species)
# #Create basisOfRecord
# dp_bats$basisOfRecord = "datapaper"
# #Create metadata of columns
# dp_bats_metadata <- data.frame(scientificName = "Species",
#                                occurrenceID = "occurrenceID",
#                                collectionCode = NA,
#                                catalogNumber = NA,
#                                decimalLongitude = "Longitude",
#                                decimalLatitude = "Latitude",
#                                coordinateUncertaintyInMeters = NA,
#                                elevation = "Altitude",
#                                country = "Country",
#                                stateProvince = "State",
#                                municipality = "Municipality",
#                                locality = "Study_location",
#                                year = "Year_finish",
#                                eventDate = NA,
#                                recordedBy = NA,
#                                identifiedBy = NA,
#                                basisOfRecord = "basisOfRecord",
#                                datasetName = NA,
#                                datasetKey = NA,
#                                key = NA, occurrenceRemarks = NA,
#                                habitat = NA)
# #Fix columns
# dp_bats_fixed <- format_columns(occ = dp_bats, binomial_from = "Species",
#                                 metadata = dp_bats_metadata,
#                                 data_source = "atlantic_bats")
# # Add kingdom
# dp_bats_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(dp_bats_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_bats.gz",
#        compress = "gzip", row.names = FALSE)
#
#
# #### Camtraps ####
# dp_camtrap_sites <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_CAMTRAPS_1-0_LOCATION.csv", encoding = "Latin-1")
# dp_camtrap_records <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_CAMTRAPS_1-0_RECORDS.csv")
# dp_camtrap_species <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_CAMTRAPS_1-0_SPECIES.csv")
# dp_camtrap_survey <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_CAMTRAPS_1-0_SURVEY.csv")
# #Merge data
# dp_camtrap <- left_join(dp_camtrap_records, dp_camtrap_species, by = "species_id") %>%
#   left_join(dp_camtrap_sites, join_by(survey_id == location_id)) %>%
#   left_join(dp_camtrap_survey, by = "survey_id")
# head(dp_camtrap)
# #Select only presences
# dp_camtrap <- dp_camtrap %>% filter(presence_absence == 1)
# #Create occurrence id
# dp_camtrap$occurrenceID <- 1:nrow(dp_camtrap)
# #Create basisOfRecord
# dp_camtrap$basisOfRecord <- "datapaper"
#
# #Create metadata of columns
# head(dp_camtrap)
# dp_camtrap_metadata <- data.frame(scientificName = "species_name",
#                                   occurrenceID = "occurrenceID",
#                                   collectionCode = NA,
#                                   catalogNumber = NA,
#                                   decimalLongitude = "X",
#                                   decimalLatitude = "Y",
#                                   coordinateUncertaintyInMeters = NA,
#                                   elevation = "AltitudeMax",
#                                   country = "country",
#                                   stateProvince = "state",
#                                   municipality = "municipality",
#                                   locality = "site",
#                                   year = "yearfinish",
#                                   eventDate = NA,
#                                   recordedBy = NA,
#                                   identifiedBy = NA,
#                                   basisOfRecord = "basisOfRecord",
#                                   datasetName = NA,
#                                   datasetKey = NA,
#                                   key = NA,
#                                   occurrenceRemarks = NA,
#                                   habitat = NA)
# #Fix columns
# dp_camtrap_fixed <- format_columns(occ = dp_camtrap,
#                                    binomial_from = "species_name",
#                                    metadata = dp_camtrap_metadata,
#                                    data_source = "atlantic_camtrap")
# # Add kingdom
# dp_camtrap_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(dp_camtrap_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_camtrap.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### Frugivory ####
# dp_frug <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_frugivory.csv", encoding = "Latin-1")
# head(dp_frug)
# #Get year
# dp_frug$year <- as.numeric(tidyr::extract_numeric(dp_frug$`Study reference`))
# #Rename column
# dp_frug <- dp_frug %>% mutate(reference = `Study reference`)
# #Create basisOfRecord
# dp_frug$basisOfRecord <- "datapaper"
#
# # Split animal and plants
# animal_frug <- dp_frug |> mutate(scientificName = Frugivore_Species)
# plant_frug <- dp_frug |> mutate(scientificName = Plant_Species)
#
# #Create metadata of columns
# dp_frug_metadata <- data.frame(scientificName = "scientificName",
#                                occurrenceID = "ID",
#                                collectionCode = NA,
#                                catalogNumber = NA,
#                                decimalLongitude = "Longitude",
#                                decimalLatitude = "Latitude",
#                                coordinateUncertaintyInMeters = NA,
#                                elevation = NA,
#                                country = NA,
#                                stateProvince = NA,
#                                municipality = NA,
#                                locality = "Study_Location",
#                                year = "year",
#                                eventDate = NA,
#                                recordedBy = "reference",
#                                identifiedBy = NA,
#                                basisOfRecord = "basisOfRecord",
#                                datasetName = NA,
#                                datasetKey = NA,
#                                key = NA, occurrenceRemarks = NA,
#                                habitat = NA)
# #Fix columns - Animals
# animal_frug_fixed <- format_columns(occ = animal_frug,
#                                     binomial_from = "scientificName",
#                                     metadata = dp_frug_metadata,
#                                     data_source = "atlantic_frugivory")
# # Add kingdom
# animal_frug_fixed$kingdom <- "Animalia"
#
# #Fix columns - Plants
# plant_frug_fixed <- format_columns(occ = plant_frug,
#                                     binomial_from = "scientificName",
#                                     metadata = dp_frug_metadata,
#                                     data_source = "atlantic_frugivory")
# # Add kingdom
# plant_frug_fixed$kingdom <- "Plantae"
#
# # merge
# dp_frug_fixed <- rbind(animal_frug_fixed, plant_frug_fixed)
#
# #Save
# fwrite(dp_frug_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_frugivory.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### Small Mammals ####
# dp_sm_sites <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_SM_Study_Site.csv", encoding = "Latin-1")
# colnames(dp_sm_sites)[1] <- "site_id"
# dp_sm_sites <- dp_sm_sites %>% filter(site_id != "") %>% dplyr::select(-Reference_number) %>%
#   distinct(site_id, .keep_all = TRUE)
# dp_sm_species <- fread("../napibio/Data/Animals/DataPapers_Ecology/ATLANTIC_SM_Capture.csv", encoding = "Latin-1")
# colnames(dp_sm_species)[1] <- "site_id"
# #Merge data
# dp_sm <- left_join(dp_sm_species, dp_sm_sites, by = "site_id")
# head(dp_sm)
# #Create occurrence id
# dp_sm$occurrenceID <- 1:nrow(dp_sm)
# #Create basisof record
# dp_sm$basisOfRecord <- "datapaper"
#
# #Create metadata of columns
# dp_sm_metadata <- data.frame(scientificName = "Actual_species_name",
#                              occurrenceID = "occurrenceID",
#                              collectionCode = NA,
#                              catalogNumber = NA,
#                              decimalLongitude = "Longitude",
#                              decimalLatitude = "Latitude",
#                              coordinateUncertaintyInMeters = NA,
#                              elevation = "Altitude",
#                              country = "Country",
#                              stateProvince = "State",
#                              municipality = "Municipality",
#                              locality = "Study_location",
#                              year = "Year_finish",
#                              eventDate = NA,
#                              recordedBy = NA,
#                              identifiedBy = NA,
#                              basisOfRecord = "basisOfRecord",
#                              datasetName = NA,
#                              datasetKey = NA,
#                              key = NA, occurrenceRemarks = NA,
#                              habitat = NA)
# #Fix columns
# dp_sm_fixed <- format_columns(occ = dp_sm, binomial_from = "Actual_species_name",
#                               metadata = dp_sm_metadata,
#                               data_source = "atlantic_small_mammal")
# # Add kingdom
# dp_sm_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(dp_sm_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_small_mammal.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### Non-volant mammals ####
# dp_non_vol_sites <- fread("../napibio/Data/Animals/DataPapers_Ecology/Mammals_UPRB_study_sites.csv", encoding = "Latin-1")
# dp_non_vol_sites$site <- paste("site", dp_non_vol_sites$site, sep = "_")
# dp_non_vol_species <- fread("../napibio/Data/Animals/DataPapers_Ecology/Mammals_UPRB_species.csv")
# #Convert pam to long dataframe
# dp_non_vol_species <- dp_non_vol_species %>% dplyr::select(-order, -family, - genus)
# dp_non_vol_species2 <- tidyr::pivot_longer(data = dp_non_vol_species,
#                                            cols = starts_with("site"),
#                                            names_to = "Sites") %>%
#   filter(value == 1)
#
# #Merge data
# dp_non_vol <- left_join(dp_non_vol_species2, dp_non_vol_sites, join_by(Sites == site))
# head(dp_non_vol)
# #Create occurrence id
# dp_non_vol$occurrenceID <- 1:nrow(dp_non_vol)
# #Create basisof record
# dp_non_vol$basisOfRecord <- "datapaper"
# head(data.frame(dp_non_vol))
# #Create metadata of columns
# dp_non_vol_metadata <- data.frame(scientificName = "species",
#                                   occurrenceID = "occurrenceID",
#                                   collectionCode = NA,
#                                   catalogNumber = NA,
#                                   decimalLongitude = "longitude",
#                                   decimalLatitude = "latitude",
#                                   coordinateUncertaintyInMeters = NA,
#                                   elevation = NA,
#                                   country = "country",
#                                   stateProvince = "state",
#                                   municipality = "municipality",
#                                   locality = "Study_location",
#                                   year = "year_finish",
#                                   eventDate = NA,
#                                   recordedBy = NA,
#                                   identifiedBy = NA,
#                                   basisOfRecord = "basisOfRecord",
#                                   datasetName = NA,
#                                   datasetKey = NA,
#                                   key = NA, occurrenceRemarks = NA,
#                                   habitat = NA)
# #Fix columns
# dp_non_vol_fixed <- format_columns(occ = dp_non_vol, binomial_from = "species",
#                                    metadata = dp_non_vol_metadata,
#                                    data_source = "atlantic_nonvolant_mammals")
# # Add kingdom
# dp_non_vol_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(dp_non_vol_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_nonvolant_mammals.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### ATLANTIC BIRDS ####
# ab <- atlantic_birds
# # Create occurrence id
# ab$occurrenceID <- 1:nrow(ab)
# #Create basisof record
# ab$basisOfRecord <- "datapaper"
# ab[1:100,] |> View()
# #Create metadata of columns
# ab_metadata <- data.frame(scientificName = "species",
#                           occurrenceID = "occurrenceID",
#                           collectionCode = NA,
#                           catalogNumber = NA,
#                           decimalLongitude = "longitude_x",
#                           decimalLatitude = "latitude_y",
#                           coordinateUncertaintyInMeters = NA,
#                           elevation = "altitude",
#                           country = "country",
#                           stateProvince = "state",
#                           municipality = "municipality",
#                           locality = NA,
#                           year = "year_finish",
#                           eventDate = NA,
#                           recordedBy = NA,
#                           identifiedBy = NA,
#                           basisOfRecord = "basisOfRecord",
#                           datasetName = NA,
#                           datasetKey = NA,
#                           key = NA, occurrenceRemarks = NA,
#                           habitat = "habitat")
# #Fix columns
# ab_fixed <- format_columns(occ = ab, binomial_from = "species",
#                            metadata = ab_metadata,
#                            data_source = "atlantic_birds")
# # Add kingdom
# ab_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(ab_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_birds.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### Atlantic ants ####
# a_ants <- atlantic_ants
# # Create occurrence id
# a_ants$occurrenceID <- 1:nrow(a_ants)
# #Create basisof record
# a_ants$basisOfRecord <- "datapaper"
# a_ants[1:100,] |> View()
# # Create species
# a_ants$species <- paste(a_ants$genus, a_ants$species, sep = " ")
#
# #Create metadata of columns
# a_ants_metadata <- data.frame(scientificName = "species",
#                           occurrenceID = "occurrenceID",
#                           collectionCode = NA,
#                           catalogNumber = NA,
#                           decimalLongitude = "longitude_x",
#                           decimalLatitude = "latitude_y",
#                           coordinateUncertaintyInMeters = NA,
#                           elevation = "altitude",
#                           country = "country",
#                           stateProvince = "state",
#                           municipality = "municipality",
#                           locality = "regional_name",
#                           year = "end_year",
#                           eventDate = NA,
#                           recordedBy = NA,
#                           identifiedBy = "source_citation",
#                           basisOfRecord = "basisOfRecord",
#                           datasetName = NA,
#                           datasetKey = NA,
#                           key = NA, occurrenceRemarks = NA,
#                           habitat = "habitat_description")
# #Fix columns
# a_ants_fixed <- format_columns(occ = a_ants, binomial_from = "species",
#                            metadata = a_ants_metadata,
#                            data_source = "atlantic_ants")
# # Add kingdom
# a_ants_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(a_ants_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_ants.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### Atlantic butterflies ####
# a_butterflies <- atlantic_butterflies
# # Create occurrence id
# a_butterflies$occurrenceID <- 1:nrow(a_butterflies)
# #Create basisof record
# a_butterflies$basisOfRecord <- "datapaper"
# a_butterflies[1:100,] |> View()
# # Create species
# a_butterflies$scientificName <- gsub("_", " ", a_butterflies$species)
#
# #Create metadata of columns
# a_butterflies_metadata <- data.frame(scientificName = "scientificName",
#                               occurrenceID = "occurrenceID",
#                               collectionCode = NA,
#                               catalogNumber = NA,
#                               decimalLongitude = "longitude",
#                               decimalLatitude = "latitude",
#                               coordinateUncertaintyInMeters = "precision",
#                               elevation = "altitude1km",
#                               country = "country",
#                               stateProvince = "state",
#                               municipality = "municipality",
#                               locality = "study_location",
#                               year = NA,
#                               eventDate = NA,
#                               recordedBy = NA,
#                               identifiedBy = NA,
#                               basisOfRecord = "basisOfRecord",
#                               datasetName = NA,
#                               datasetKey = NA,
#                               key = NA, occurrenceRemarks = NA,
#                               habitat = "habitat_description")
# #Fix columns
# a_butterflies_fixed <- format_columns(occ = a_butterflies, binomial_from = "species",
#                                metadata = a_butterflies_metadata,
#                                data_source = "atlantic_butterflies")
# # Add kingdom
# a_butterflies_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(a_butterflies_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_butterflies.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### atlantic_camera_trap_mammals ####
# ct <- atlantic_camera_trap_mammals
# # Extract year
# ct$year <- plantR::getYear(ct$recordDate)
# # Create occurrence id
# ct$occurrenceID <- 1:nrow(ct)
# #Create basisof record
# ct$basisOfRecord <- "datapaper"
# #Create metadata of columns
# ct_metadata <- data.frame(scientificName = "scientificName",
#                                      occurrenceID = "occurrenceID",
#                                      collectionCode = NA,
#                                      catalogNumber = NA,
#                                      decimalLongitude = "decimalLongitude",
#                                      decimalLatitude = "decimalLatitude",
#                                      coordinateUncertaintyInMeters = NA,
#                                      elevation = NA,
#                                      country = NA,
#                                      stateProvince = NA,
#                                      municipality = NA,
#                                      locality = NA,
#                                      year = "year",
#                                      eventDate = NA,
#                                      recordedBy = NA,
#                                      identifiedBy = NA,
#                                      basisOfRecord = "basisOfRecord",
#                                      datasetName = NA,
#                                      datasetKey = NA,
#                                      key = NA, occurrenceRemarks = NA,
#                                      habitat = NA)
# #Fix columns
# ct_fixed <- format_columns(occ = ct, binomial_from = "scientificName",
#                            metadata = ct_metadata,
#                            data_source = "atlantic_camera_trap_mammals")
# # Add kingdom
# ct_fixed$kingdom <- "Animalia"
#
# #Save
# fwrite(ct_fixed,
#        "../RuHere_paper/AtlanticR/atlantic_camera_trap_mammals.gz",
#        compress = "gzip", row.names = FALSE)
#
# # Merge all data
# lf <- list.files("../RuHere_paper/AtlanticR/", pattern = ".gz", full.names = TRUE)
# lf
# d <- pblapply(lf, fread, encoding = "Latin-1")
# d2 <- rbindlist(d, use.names=TRUE)
# # Remove records without species name, longitude or latitude
# d3 <- d2 |>
#   filter(!is.na(species), species != "",
#          !is.na(decimalLongitude), decimalLongitude != "",
#          !is.na(decimalLatitude), decimalLatitude != "")
# # Fix occurrence id and basis of records
# d3$occurrenceID <- 1:nrow(d3)
# d3$basisOfRecord <- "datapaper"
# d3$data_source |> table()
#
# # Save
# fwrite(d3,
#        "../RuHere_paper/AtlanticR/atlantic_data_papers.gz")
#
# # Test
# araucaria <- d3 |> filter(species == "Araucaria angustifolia")
# pts <- spatialize(araucaria)
# mapview::mapview(pts)

# #### DRYFLOR ####
# # Plant diversity patterns in neotropical dry forests and their conservation implications. DRYFLOR 2016. Science 355: 465-466
# #http://www.dryflor.info/data/datasets
# #Import data
# #Import data
# df <- fread("../napibio/Data/Plants/DryFlor/dryflor_Science_rec_gr.csv")
# #Get species
# df$species <- paste(df$genus, df$epitet)
# #Convert do DWC
# #Create columns
# df$occurrenceID <- 1:nrow(df)
# df$basisOfRecord <- "datapaper"
# #Fix country
# df_country <- data.frame(Cou = c("Arg", "Bah", "Bol", "Bra", "Col", "Cos",
#                                  "Cub", "Dom", "Ecu", "Jam", "Mex", "Nic",
#                                  "Par", "Per", "Pue", "Sai", "Tri", "Ven",
#                                  "Vir"),
#                          Country = c("Argentina", "Bahamas", "Bolivia",
#                                      "Brazil", "Colombia", "Costa Rica", "Cuba",
#                                      "Dominican Republic", "Ecuador", "Jamaica",
#                                      "Mexico", "Nicaragua", "Paraguai", "Peru",
#                                      "Puerto Rico", "Saint Lucia",
#                                      "Trinidad and Tobago", "Venezuela",
#                                      "Virgin Islands"))
# df <- left_join(df, df_country, by = "Cou")
# #Columns metadata
# colnames(df)
# df_metadata <- data.frame(scientificName = "species",
#                           occurrenceID = "occurrenceID",
#                           collectionCode = NA,
#                           catalogNumber = NA,
#                           decimalLongitude = "Long",
#                           decimalLatitude = "Lat",
#                           coordinateUncertaintyInMeters = NA,
#                           elevation = "Alt",
#                           country = "Country",
#                           stateProvince = NA,
#                           municipality = NA,
#                           locality = NA,
#                           year = "year",
#                           eventDate = NA,
#                           recordedBy = NA,
#                           identifiedBy = NA,
#                           basisOfRecord = "basisOfRecord",
#                           datasetName = NA,
#                           datasetKey = NA,
#                           key = NA,
#                           occurrenceRemarks = NA,
#                           habitat = "SiteCode")
# #Fix columns
# df_fixed <- format_columns(occ = df, binomial_from = "species",
#                            metadata = df_metadata,
#                            data_source = "dryflor")
#
# # Add kingdom
# df_fixed$kingdom <- "Plantae"
#
# #Save
# dir.create("../RuHere_paper/dryflor/")
# fwrite(df_fixed,
#        "../RuHere_paper/dryflor/DryFlor_RuHere.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### NEOTROPICTREE ####
# #Download data from: https://data.mendeley.com/datasets/jmv8bn8fwc/2
# #Merge dataset
# nt_tree <- fread("c:/Users/wever/Downloads/data_species_tree.csv", header = T)
# #Wider to longer format
# nt_tree <- tidyr::pivot_longer(data = nt_tree, !id) %>%
#   filter(!is.na(value),
#          value != "") %>%
#   dplyr::select(code = id, species = value)
# #Import sites information
# nt_sites <-  fread("c:/Users/wever/Downloads/data_environment_treespecies.csv",
#                    header = T, encoding = "Latin-1")
# #Convert coordinates to decimal degrees
# convert_dms_to_dd <- function(data, longitude = "longitude", latitude = "latitude") {
#   #Get longitude
#   x = data[[longitude]]
#   y = data[[latitude]]
#
#   #Split degrees, minutes and seconds
#   x_dms <- extract_dms(x)
#   #Fix: All x directions are W
#   x_dms$direction <- "W"
#   y_dms <- extract_dms(y)
#
#   # Convert to decimal degrees
#   dd_x <- x_dms[["degrees"]] + x_dms[["minutes"]] / 60 + x_dms[["seconds"]] / 3600
#   dd_y <- y_dms[["degrees"]] + y_dms[["minutes"]] / 60 + y_dms[["seconds"]] / 3600
#   #Fix direction
#   dd_x[x_dms$direction == "W"] <- dd_x[x_dms$direction == "W"] * -1
#   dd_y[y_dms$direction == "S"] <- dd_y[y_dms$direction == "S"] * -1
#
#   #Bind columns to data
#   d <- data %>%
#     mutate(decimalLongitude = dd_x, .after = longitude) %>%
#     mutate(decimalLatitude = dd_y, .after = latitude)
#   return(d)
# }
# nt_sites <- convert_dms_to_dd(data = nt_sites)
# #Join data
# nt <- left_join(nt_tree, nt_sites, by = "code")
# #Create columns
# nt$occurrenceID <- 1:nrow(nt)
# nt$basisOfRecord <- "checklist"
# nt$coordinateUncertaintyInMeters <- 5000 #Neotropictree is a checklist of species in each site, with each site being a circular area with a radius of 5 km
# nt$year <- 2017
# #Fix country - some sites have more than one country
# nt$country <- sub("/.*", "", nt$country)
# #Fix encoding with stringi::stri_enc_toutf8()
# check_columns <- c("country", "state", "sitename")
# for(i in check_columns){
#   print(i)
#   nt[[i]] <- stringi::stri_enc_toutf8(nt[[i]])
# }
#
# #Columns metadata
# nt_metadata <- data.frame(scientificName = "species",
#                           occurrenceID = "occurrenceID",
#                           collectionCode = NA,
#                           catalogNumber = NA,
#                           decimalLongitude = "decimalLongitude",
#                           decimalLatitude = "decimalLatitude",
#                           coordinateUncertaintyInMeters = "coordinateUncertaintyInMeters",
#                           elevation = "altitude(m)",
#                           country = "country",
#                           stateProvince = "state",
#                           municipality = NA,
#                           locality = "sitename",
#                           year = "year",
#                           eventDate = NA,
#                           recordedBy = NA,
#                           identifiedBy = NA,
#                           basisOfRecord = "basisOfRecord",
#                           datasetName = NA,
#                           datasetKey = NA,
#                           key = NA,
#                           occurrenceRemarks = NA,
#                           habitat = "vegetation type")
#
# #Fix columns
# nt_fixed <- format_columns(occ = nt, binomial_from = "species",
#                            metadata = nt_metadata,
#                            data_source = "NeoTropTree")
# # Add kingdom
# nt_fixed$kingdom <- "Plantae"
#
# #Save
# dir.create("../RuHere_paper/NeoTropTree/")
# fwrite(nt_fixed,
#        "../RuHere_paper/NeoTropTree/NeoTropTree_RuHere.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### BRAZIL ROADKILL ####
# # Download from: https://esajournals.onlinelibrary.wiley.com/doi/10.1002/ecy.2464
# r <- fread("../RuHere_paper/roadkill/Brazil_Roadkill_20180527.csv", encoding = "Latin-1")
# # Create occurrence id
# r$occurrenceID <- 1:nrow(r)
# #Create basisof record
# r$basisOfRecord <- "datapaper"
# # Create country
# r$country <- "Brazil"
#
# r[1:100,] |> View()
# #Create metadata of columns
# r_metadata <- data.frame(scientificName = "Scientific_name",
#                           occurrenceID = "occurrenceID",
#                           collectionCode = NA,
#                           catalogNumber = NA,
#                           decimalLongitude = "Long",
#                           decimalLatitude = "Lat",
#                           coordinateUncertaintyInMeters = NA,
#                           elevation = NA,
#                           country = "country",
#                           stateProvince = NA,
#                           municipality = NA,
#                           locality = "Road_ID",
#                           year = "Year",
#                           eventDate = NA,
#                           recordedBy = "Reference_ID",
#                           identifiedBy = NA,
#                           basisOfRecord = "basisOfRecord",
#                           datasetName = NA,
#                           datasetKey = NA,
#                           key = NA, occurrenceRemarks = NA,
#                           habitat = "Road")
# #Fix columns
# r_fixed <- format_columns(occ = r, binomial_from = "Scientific_name",
#                           metadata = r_metadata,
#                           data_source = "Brazil road-kill")
# # Add kingdom
# r_fixed$kingdom <- "Animalia"
#
# # Get other taxonomic information
# tax_info <- r |>
#   select(species = Scientific_name, Common_name, Genus, Family, Order, Class) |>
#   distinct(species, .keep_all = TRUE)
# r_fixed2 <- left_join(r_fixed, tax_info, by = "species")
#
# # Order
# o <- c("record_id", "occurrenceID", "kingdom", "Class", "Order", "Family", "Genus", "species",
#        "scientificName", "Common_name", "collectionCode", "catalogNumber", "decimalLongitude",
#        "decimalLatitude", "coordinateUncertaintyInMeters",
#        "elevation", "country", "stateProvince", "municipality", "locality",
#        "year", "eventDate", "recordedBy", "identifiedBy", "basisOfRecord",
#        "datasetName", "datasetKey", "key", "occurrenceRemarks", "habitat",
#        "data_source")
# r_fixed2 <- r_fixed2[, o]
#
# #Save
# dir.create("../RuHere_paper/roadkill/")
# fwrite(r_fixed2,
#        "../RuHere_paper/roadkill/Brazil_road-kill_RuHere.gz",
#        compress = "gzip", row.names = FALSE)
#
# #### NEOTROPICAL XENARTHRANS ####
# # Import data
# quali <- fread("../RuHere_paper/Xenarthrans/NEOTROPICAL_XENARTHRANS_QUALITATIVE.csv",
#                encoding = "Latin-1")
# # Create occurrence id
# quali$occurrenceID <- 1:nrow(quali)
# #Create basisof record
# quali$basisOfRecord <- "datapaper"
# quali[1:100,] |> View()
#
# #Create metadata of columns
# quali_metadata <- data.frame(scientificName = "SPECIES",
#                          occurrenceID = "occurrenceID",
#                          collectionCode = NA,
#                          catalogNumber = NA,
#                          decimalLongitude = "LONG_X",
#                          decimalLatitude = "LAT_Y",
#                          coordinateUncertaintyInMeters = "PRECISION",
#                          elevation = "ALTITUDE",
#                          country = "COUNTRY",
#                          stateProvince = "STATE",
#                          municipality = "MUNICIPALITY",
#                          locality = "SITE",
#                          year = NA,
#                          eventDate = NA,
#                          recordedBy = "REFERENCE",
#                          identifiedBy = NA,
#                          basisOfRecord = "basisOfRecord",
#                          datasetName = NA,
#                          datasetKey = NA,
#                          key = NA, occurrenceRemarks = NA,
#                          habitat = "VEG_TYPE")
# #Fix columns
# quali_fixed <- format_columns(occ = quali, binomial_from = "SPECIES",
#                               metadata = quali_metadata,
#                               data_source = "Neotropical Xenarthrans")
# # Add kingdom
# quali_fixed$kingdom <- "Animalia"
#
# # Get other taxonomic information
# tax_info <- quali |>
#   select(species = SPECIES, order = ORDER, family = FAMILY, genus = GENUS) |>
#   distinct(species, .keep_all = TRUE)
# quali_fixed2 <- left_join(quali_fixed, tax_info, by = "species")
#
# # Order
# colnames(quali_fixed) |> dput()
# o <- c("record_id", "occurrenceID", "kingdom", "order", "family", "genus", "species",
#        "scientificName", "collectionCode", "catalogNumber", "decimalLongitude",
#        "decimalLatitude", "coordinateUncertaintyInMeters",
#        "elevation", "country", "stateProvince", "municipality", "locality",
#        "year", "eventDate", "recordedBy", "identifiedBy", "basisOfRecord",
#        "datasetName", "datasetKey", "key", "occurrenceRemarks", "habitat",
#        "data_source")
# quali_fixed2 <- quali_fixed2[, o]
#
# #Save
# fwrite(quali_fixed2,
#        "../RuHere_paper/Xenarthrans//Neotropical_Xenarthrans_RuHere.gz",
#        compress = "gzip", row.names = FALSE)

