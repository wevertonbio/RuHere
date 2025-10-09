#### Data creation ####
# Scripts for data creation

#### Metadata for GBIF, SpeciesLink, BIEN, etc ####
gbif_metadata <- data.frame(scientificName = "acceptedScientificName", #Nome científico
                            collectionCode = "collectionCode",
                            catalogNumber = "catalogNumber",
                            decimalLongitude = "decimalLongitude", #Longitude
                            decimalLatitude = "decimalLatitude", #Latitude
                            coordinateUncertaintyInMeters = "coordinateUncertaintyInMeters", #Incerteza
                            elevation = "elevation", #Altitude
                            country = "countryCode", #País
                            stateProvince = "stateProvince", #Estado
                            municipality = "municipality", #Município
                            locality = "locality", #Local
                            year = "year", #Ano
                            eventDate = "eventDate", #Data de coleta
                            recordedBy = "recordedBy", #Coletor
                            identifiedBy = "identifiedBy", #Identificador
                            basisOfRecord = "basisOfRecord", #Tipo de registro (Preserved specimen, human observation, etc)
                            occurrenceRemarks = "occurrenceRemarks", #Observações sobre ocorrencia
                            habitat = "habitat", #Habitat onde foi encontrado
                            datasetName = "datasetName", #Origem dos dados (ex: herbário X)
                            datasetKey ="datasetKey", #Chave da origem dos dados
                            key = "speciesKey")

# Criar dataframe com metadados identificando as colunas
splink_metadata <- data.frame(scientificName = "scientificname", #Nome científico
                              collectionCode = "collectioncode",
                              catalogNumber = "catalognumber",
                              decimalLongitude = "decimallongitude", #Longitude
                              decimalLatitude = "decimallatitude", #Latitude
                              coordinateUncertaintyInMeters = NA, #Incerteza da coordenada
                              elevation = "maximumelevationinmeters", #Altitude
                              country = "country", #País
                              stateProvince = "stateprovince", #Estado
                              municipality = "county", #Município
                              locality = "locality", #Local
                              year = "yearcollected", #Ano
                              eventDate = NA, #Data de coleta
                              recordedBy = "recordedby", #Coletor
                              identifiedBy = "identifiedby", #Identificador
                              basisOfRecord = "basisofrecord", #Tipo de registro (Preserved specimen, human observation, etc)
                              occurrenceRemarks = "occurrenceremarks", #Observações sobre ocorrencia
                              habitat = NA, #Habitat onde foi encontrado
                              datasetName = NA, #Origem dos dados (ex: herbário X)
                              datasetKey = NA, #Chave da origem dos dados
                              key = NA) #Chave da espécie no GBIF
# Criar metadados
idigbio_metadata <- data.frame(scientificName = "scientificname",
                          collectionCode = "collectioncode",
                          catalogNumber = "catalognumber",
                          decimalLongitude = "geopoint.lon",
                          decimalLatitude = "geopoint.lat",
                          coordinateUncertaintyInMeters = "coordinateuncertainty",
                          elevation = NA,
                          country = "country",
                          stateProvince = "stateprovince",
                          municipality = "municipality",
                          locality = "locality",
                          year = "year",
                          eventDate = "datecollected",
                          recordedBy = "collector",
                          identifiedBy = NA,
                          basisOfRecord = "basisofrecord",
                          occurrenceRemarks = NA,
                          habitat = NA,
                          datasetName = NA,
                          datasetKey = "datasetid",
                          key = NA)
bien_metadata <- data.frame(scientificName = "scrubbed_species_binomial",
                            collectionCode = "collection_code",
                            catalogNumber = NA,
                            decimalLongitude = "longitude",
                            decimalLatitude = "latitude",
                            coordinateUncertaintyInMeters = NA,
                            elevation = "elevation_m",
                            country = "country",
                            stateProvince = "state_province",
                            municipality = "county",
                            locality = "locality",
                            year = "date_collected",
                            eventDate = "date_collected",
                            recordedBy = "recorded_by",
                            identifiedBy = "identified_by",
                            basisOfRecord = "observation_type",
                            occurrenceRemarks = NA,
                            habitat = NA,
                            datasetName = "datasource",
                            datasetKey = NA,
                            key = NA)
# Save in a list
prepared_metadata <- list("gbif" = gbif_metadata,
                          "specieslink" = splink_metadata,
                          "idigbio" = idigbio_metadata,
                          "bien" = bien_metadata)
usethis::use_data(prepared_metadata, overwrite = TRUE)

#### Occurrences of pau-brasil from specieslink, gbif and bien ####
# Splink
occ_splink <- get_specieslink(species = "Paubrasilia echinata",
                              limit = 10000, save = FALSE)
usethis::use_data(occ_splink)
# BIEN
occ_bien <- get_bien(
  by = "species",
  species = "Paubrasilia echinata",
  cultivated = TRUE,
  native.status = TRUE,
  observation.type = TRUE,
  only.geovalid = T,
  political.boundaries = TRUE, collection.info = TRUE)

usethis::use_data(occ_bien, overwrite = TRUE)
# GBIF
gbif_prepared <- prepare_gbif_download(species = "Paubrasilia echinata")
req_gbif <- request_gbif(gbif_info = gbif_prepared)
rgbif::occ_download_wait(req_gbif)
occ_gbif <- import_gbif(request_key = req_gbif, write_file = FALSE,
                        select_columns = FALSE,
                        overwrite = TRUE)
usethis::use_data(occ_gbif, overwrite = TRUE)


#### List of countries ####
# Get all countries
cc <- rnaturalearthdata::map_units110 %>% as.data.frame()
cc <- cc %>% select(starts_with("name"),
                    sovereignt, admin, geounit, subunit, name, name_long,
                    brk_name, abbrev, formal_en, name_ciawf, name_sort,
                    sov_a3, adm0_a3, gu_a3, su_a3, brk_a3, postal,
                    fips_10, iso_a2, iso_a2_eh, iso_a3, iso_a3_eh)
# Create dictionary
colunas_nomes <- c("sovereignt", "admin", "geounit", "subunit", "name",
                   "name_long", "brk_name",
                   "abbrev", "formal_en", "name_ciawf", "name_sort",
                   "name_alt", "name_ar", "name_bn", "name_de",
                   "name_en", "name_es", "name_fa", "name_fr", "name_el",
                   "name_he", "name_hi", "name_hu", "name_id", "name_it",
                   "name_ja", "name_ko", "name_nl", "name_pl", "name_pt",
                   "name_ru", "name_sv", "name_tr", "name_uk", "name_ur",
                   "name_vi", "name_zh", "name_zht")

colunas_codigos <- c("sov_a3", "adm0_a3", "gu_a3", "su_a3", "brk_a3", "postal",
                     "fips_10", "iso_a2", "iso_a2_eh", "iso_a3", "iso_a3_eh")

dicionario_nomes <- lapply(colunas_nomes, function(i){
  cc %>% select(country_name = i, country_suggested = name) %>%
    mutate(country_name = tolower(country_name),
           country_suggested = tolower(country_suggested))
}) %>% data.table::rbindlist() %>% as.data.frame() %>% distinct()
# Remover acentos e caracteres especiais
dicionario_nomes <- dicionario_nomes %>%
  mutate(country_name = remove_accent(country_name),
         country_suggested = remove_accent(country_suggested)) %>%
  distinct()

# Codigos
dicionario_codigos <- lapply(colunas_codigos, function(i){
  cc %>% select(country_code = i, country_suggested = name) %>%
    mutate(country_code = toupper(country_code),
           country_suggested = tolower(country_suggested))
}) %>% data.table::rbindlist() %>% as.data.frame() %>% distinct()
# Filter
dicionario_codigos <- dicionario_codigos %>%
  filter(country_code != -99,
         country_code != "J",
         !(country_code == "IS" & country_suggested == "iceland"),
         !(country_code == "CN" & country_suggested == "n. cyprus"),
         !(country_code == "ES" & country_suggested == "eswatini"),
         !(country_code == "SL" & country_suggested == "somaliland"))
dicionario_codigos <- dicionario_codigos %>%
  filter(!(country_code%in% c("FR1", "FR", "FRA") &
             country_suggested != "france"),
         !(country_code %in% c("US1", "US") &
             country_suggested != "united states of america"),
         !(country_code %in% c("GB", "GBR", "GB1") &
             country_suggested != "england"),
         !(country_code %in% c("NOR") &
             country_suggested != "norway"),
         !(country_code %in% c("PNG") &
             country_suggested != "papua new guinea"),
         !(country_code %in% c("DN1") &
             country_suggested != "denmark"),
         !(country_code %in% c("IS1") &
             country_suggested != "israel"),
         !(country_code %in% c("BO") &
             country_suggested != "bolivia"),
         !(country_code %in% c("TT") &
             country_suggested != "trinidad and tobago"))

# Check duplicates
dicionario_codigos %>% filter(duplicated(country_code))
# Remove duplicated codes
dc <- dicionario_codigos %>% filter(duplicated(country_code)) %>%
  pull(country_code) %>% unique()
dicionario_codigos <- dicionario_codigos %>% filter(!(country_code %in% dc)) %>%
  distinct()
# Remover acentos e caracteres especiais
dicionario_codigos <- dicionario_codigos %>%
  mutate(country_code = remove_accent(country_code),
         country_suggested = remove_accent(country_suggested)) %>%
  distinct()

# Save as country_dictionary
country_dictionary <- list(country_name = dicionario_nomes,
                           country_code = dicionario_codigos)
usethis::use_data(country_dictionary, overwrite = TRUE)

#### List of states ####
# Get all states
ss <- rnaturalearthdata::states50 %>% as.data.frame()
ss <- ss %>% select(starts_with("name"),
                    code_hasc, postal, abbrev, woe_name, gn_name,
                    country = geonunit)
# Create dictionary
colunas_nomes <- c("name", "name_alt", "name_local", "name_ar",
                   "name_bn", "name_de", "name_en", "name_es", "name_fr",
                   "name_el", "name_hi", "name_hu", "name_id", "name_it",
                   "name_ja", "name_ko", "name_nl", "name_pl", "name_pt",
                   "name_ru", "name_sv", "name_tr", "name_vi", "name_zh",
                   "name_he", "name_uk", "name_ur", "name_fa", "name_zht",
                   "abbrev", "woe_name", "gn_name")

colunas_codigos <- c("postal")

dicionario_nomes_states <- lapply(colunas_nomes, function(i){
  ss %>% select(state_name = i, state_suggested = name, country) %>%
    mutate(state_name = tolower(state_name),
           state_suggested = tolower(state_suggested),
           country = tolower(country))
}) %>% data.table::rbindlist() %>% as.data.frame() %>% distinct()
# Remover acentos e caracteres especiais
dicionario_nomes_states <- dicionario_nomes_states %>%
  mutate(state_name = remove_accent(state_name),
         state_suggested = remove_accent(state_suggested),
         country = remove_accent(country)) %>%
  distinct() %>% na.omit()

# Codigos
dicionario_codigos_states <- lapply(colunas_codigos, function(i){
  ss %>% select(state_code = i, state_suggested = name, country) %>%
    mutate(state_code = toupper(state_code),
           state_suggested = tolower(state_suggested),
           country = tolower(country))
}) %>% data.table::rbindlist() %>% as.data.frame() %>% distinct()
# Check duplicates
dicionario_codigos_states %>% filter(duplicated(state_code, country))
# Remover acentos e caracteres especiais
dicionario_codigos_states <- dicionario_codigos_states %>%
  mutate(state_code = remove_accent(state_code),
         state_suggested = remove_accent(state_suggested),
         country = remove_accent(country)) %>%
  distinct()

# Save as state_dictionary
states_dictionary <- list(states_name = dicionario_nomes_states,
                          states_code = dicionario_codigos_states)
usethis::use_data(states_dictionary, overwrite = TRUE)

#### Countries and states ####
library(terra)
countries <- rnaturalearthdata::map_units110 %>% vect()
names(countries)
countries <- countries[,"name"]
countries$name <- tolower(countries$name)
countries$name <- remove_accent(countries$name)
plot(countries)
mapview(countries)
dir.create("inst/extdata", recursive = TRUE)
writeVector(countries, "inst/extdata/world.gpkg", overwrite = TRUE)
# States
states <- rnaturalearthdata::states50 %>% vect()
rnaturalearthdata::states50 %>% View()
names(states)
states <- states[,"name"]
states$name <- tolower(states$name)
states$name <- remove_accent(states$name)
plot(states)
mapview(states)
dir.create("inst/extdata", recursive = TRUE)
writeVector(states, "inst/extdata/states.gpkg", overwrite = TRUE)
