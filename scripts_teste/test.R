#### Script for test data ####
devtools::load_all()
library(data.table)
library(dplyr)

#### Get data from GBIF ####
# Prepare data (check id and number of records)
gbif_prepared <- prepare_gbif_download(species = c("Araucaria angustifolia",
                                                   "Paubrasilia echinata"))
# Request data
req_gbif <- request_gbif(gbif_info = gbif_prepared)
rgbif::occ_download_wait(req_gbif)

# After processing, import data
occ_gbif <- import_gbif(request_key = req_gbif, write_file = TRUE,
                        output_dir = "../RuHere_test/", file.format = "gz",
                        select_columns = FALSE, overwrite = TRUE)
occ_gbif$scientificName %>% unique()
occ_gbif$acceptedScientificName %>% unique() #Arrumar isso

occ_gbif$acceptedNameUsage %>% unique() %>% florabr::get_binomial()
occ_gbif %>% filter(acceptedNameUsage == "") %>%
  select(scientificName, acceptedScientificName, acceptedNameUsage) %>% View()

#### Get data from Specieslink ####
occ_splink <- get_specieslink(species = c("Araucaria angustifolia",
                                          "Paubrasilia echinata"),
                              save = FALSE, limit = 10000)
occ_splink$scientificname %>% unique()

#### Get data from BIEN ####
occ_bien <- get_bien(by = "species",
                     species = c("Araucaria angustifolia",
                                 "Paubrasilia echinata"),
                     cultivated = FALSE,
                     native.status = TRUE,
                     observation.type = TRUE,
                     only.geovalid = T)
occ_bien$scrubbed_species_binomial %>% unique()

#### Get data from idigbio ####
occ_idig <- get_idigbio(type = "api_records",
                        rq=list(scientificname="Araucaria angustifolia",
                        compress = T))
occ_idig$scientificName %>% unique()

tt <- ridigbio::idig_search_records(rq=list(scientificname="Araucaria angustifolia"))


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
# iDigBio
# Não funciona por enquanto - ver comentários na função
# occ <- RuHere::occ_idig
# occ_idig <- format_columns(occ = occ, metadata = "idigbio")

# Merge data with bind_rows
all_occ <- bind_rows(occ_splink, occ_gbif, occ_bien)

#### Bind ocurrences ####
all_occ <- bind_here(occ_splink, occ_gbif, occ_bien, fake_data)

# Save to use in package
occurrences <- all_occ
usethis::use_data(occurrences, overwrite = TRUE)

#### Standardize countries ####
res <- standardize_countries(all_occ,
                             lookup_na_country = TRUE,
                             long = "decimalLongitude", lat = "decimalLatitude",
                             return_dictionary = F)
View(RuHere::country_dictionary$country_name)
res$occ %>% select(country, country_suggested, country_source) %>% View()
res$report %>% View()

#### Standardize States ####
res <- standardize_states(res, return_dictionary = F,
                          lookup_na_state = TRUE,
                          long = "decimalLongitude", lat = "decimalLatitude")
res %>% select(stateProvince, state_suggested, state_source) %>% View()
View(RuHere::states_dictionary$states_name)
table(RuHere::states_dictionary$states_name$country)

#### Check and fix coutries####
res_countries <- check_countries(occ = res,
                                 country_column = "country_suggested",
                                 try_to_fix = TRUE)
res_countries %>% select(country_suggested, country_issues) %>% View()
res_countries$country_issues %>% table()

#### Check and fix states ####
res_states <- check_states(occ = res,
                           state_column = "state_suggested",
                           try_to_fix = TRUE)
View(RuHere::fake_data)

#### Get data from WCVP ####
wcvp_here(data_dir = "../RuHere_test/")
# See data
wcvp <- fread("../RuHere_test/wcvp/wcvp.gz")
wcvp %>% filter(species == "Araucaria angustifolia") %>% View()
wcvp_map <- vect("../RuHere_test/wcvp/wgsrpd.gpkg")
mapview::mapview(wcvp_map)

#### Get data from BIEN ####
bien_here(data_dir = "../RuHere_test/",
          species = c("Araucaria angustifolia",
                      "Paubrasilia echinata"),
          synonyms = RuHere::synonyms)
# See data
bien_range <- vect("../RuHere_test/bien/Paubrasilia echinata.gpkg")
mapview::mapview(bien_range)

#### Get data from IUCN ####
# Precisa checar se mapa está em wcvp, se não estiver, baixar
d <- iucn_here(species = c("Araucaria angustifolia",
                           "Paubrasilia echinata"),
              synonyms = RuHere::synonyms,
              iucn_credential = NULL, #Salvo no Renviren
              data_dir = "../RuHere_test/",
              return_data = TRUE)
d <- fread("../RuHere_test/iucn/iucn_distribution.gz")

#### Get data from florabr ####
florabr_here(data_dir = "../RuHere_test/",
             data_version = "latest",
             overwrite = TRUE, verbose = TRUE)
# See data
floradata <- florabr::load_florabr("../RuHere_test/florabr")


# Example on how to filter
res$species <- "Paubrasilia echinata" # Alguma função ou maneira de padronizar nomes???
floradata %>% filter(species == "Paubrasilia echinata") %>%
  select(species, biome, states, endemism) %>% View()

filterflora <- florabr::filter_florabr(data = floradata, occ = res,
                                       species = "species",
                                       long = "decimalLongitude",
                                       lat = "decimalLatitude",
                                       by_endemism = TRUE)
filterflora$flagged %>%
  select(record_id, filters_ok) %>%
  View()

#### Get data from faunabr ####
faunabr_here(data_dir = "../RuHere_test/",
             data_version = "latest",
             overwrite = TRUE, verbose = TRUE)
# See data
faunadata <- faunabr::load_faunabr("../RuHere_test/faunabr")
faunadata %>% filter(species == "Panthera onca") %>%
  select(species, states, countryCode) %>% View()
?faunabr::filter_faunabr
faunabr::fauna_synonym(data = faunadata, species = "Panthera onca")

#### Dados ####
list.files("data")
RuHere::country_dictionary$country_name %>% tibble()
RuHere::country_dictionary$country_code %>% tibble()
RuHere::states_dictionary$states_name %>% tibble()
RuHere::states_dictionary$states_code %>% tibble()
# Dados espaciais
states <- vect("inst/extdata/states.gpkg")
mapview::mapview(states)
contries <- vect("inst/extdata/world.gpkg")
mapview::mapview(contries)
