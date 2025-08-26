# Aquisição e preparo de dados primários de biodiversidade ####

#### DOWNLOAD DE REGISTROS DE OCORRÊNCIA ####

#Carregar pacotes
library(rgbif) #pacote para baixar dados do gbif
library(data.table) #Importar e salvar tabelas
library(terra)
library(mapview)
library(dplyr)
library(stringr)

#### Download de registros do GBIF ####

#Crie uma conta no GBIF: https://www.gbif.org/

#Podemos baixar ocorrencias direto do gbif:
# Acesse: https://www.gbif.org/species/2684940


#Para baixar pelo R, precisamos da chave da espécie.

# DICA: CRIE UM OBJETO COM O NOME DA ESPÉCIE!
# Esse objeto sera usado para criar pastas e baixar as ocorrências
sp <- "Araucaria angustifolia"
#Criar diretório para salvar ocorrencias
sp_dir <- file.path("Ocorrencias", sp)
sp_dir

# Checar chave (key) da espécie no gbif
sp_info <- name_backbone(name = sp,
                         kingdom = "plants") #Veja outras opções na função
#Obter chave
k <- sp_info$usageKey
k

#Obter numero de ocorrências
n_occ <- occ_count(taxonKey = k)
n_occ

n_occ_with_coordinates <- rgbif::occ_count(taxonKey = k,
                                           hasCoordinate = TRUE)
n_occ_with_coordinates

#Fazer download com GBIF
# Precisa de username, senha e email do GBIF
# Dica: salve essas informações no seu R environment permanentemente
# Abra seu arquivo do R environment
usethis::edit_r_environ()
# No arquivo, adicione essas linhas e preencha com suas informações:
GBIF_USERNAME=""
GBIF_EMAIL=""
GBIF_PASSWORD=""
#Salve o arquivo, feche e reinicie o R (Session > Restart R)
# Carregue os pacotes novamente
# Teste para ver se R armazenou informações
Sys.getenv("GBIF_USERNAME") #Username de registro no gbif
Sys.getenv("GBIF_EMAIL") #Email

occ_request <- occ_download(
  pred_in("taxonKey", k), #Taxonkey
  pred("hasCoordinate", TRUE), #Com coordenadas
  pred("hasGeospatialIssue", FALSE), #Sem problemas geospaciais
  format = "DWCA",
  user = Sys.getenv("GBIF_USERNAME"),
  pwd = Sys.getenv("GBIF_PASSWORD"),
  email = Sys.getenv("GBIF_EMAIL"))

#Checar status de download
occ_request
occ_download_wait(occ_request[1])
#Aguarde cerca de 5-15 minutos
#Após status mudar para SUCCEEDED, podemos baixar diretamente do site ou pelo R
dir.create(file.path(sp_dir, "GBIF"), recursive = TRUE)
occ_d <- occ_download_get(key = occ_request, #Cole a chave aqui ou o objeto occ_request
                          path = file.path(sp_dir, "GBIF"),
                          overwrite=TRUE)
#Importar para R
occ <- occ_download_import(occ_d)
colnames(occ) #Ver colunas

#Salvar tabela no formato csv (Para importar, use read.csv)
write.csv(occ,
          file.path(sp_dir, "GBIF/Ocorrencias.csv"), #Pasta e nome do arquivo
          row.names = FALSE)
# Dica: se quiser abrir o arquivo no Excel, use write.csv2 (Para importar, use red.csv2)
write.csv2(occ,
           file.path(sp_dir, "GBIF/Ocorrencias_2.csv"), #Pasta e nome do arquivo
           row.names = FALSE)

#Salvar tabela no formato gz (comprimido) (Para importar, use fread do pacote data.table)
fwrite(occ,
       file.path(sp_dir, "GBIF/Ocorrencias.gz"), #Pasta e nome do arquivo
       compress = "gzip", row.names = FALSE)
# Compare o tamanho dos arquivos
# Qual formato usar?
# Use csv se quiser abrir em outro programa (qgis ou excel)
# Use gz se for abrir o arquivo apenas no R (economiza espaço em disco)

#Importar
occ2 <- fread(file.path(sp_dir, "GBIF/Ocorrencias.gz"))

#Espacializar pontos
occ_gbif <- vect(occ2,
                 geom = c(x = "decimalLongitude", y = "decimalLatitude"),
                crs = "epsg:4326")
plot(occ_gbif) #Plot estático
mapview(occ_gbif)

#### Download de registros do speciesLink ####
# Download direto pelo site
# https://specieslink.net/search/

#Carregar pacotes
library(jsonlite) #pacote para baixar dados do specieslink
# Importar função para baixar dados do splink
source("scripts/helpers/rspecieslink2.R")

#Crie uma conta no SpeciesLink: https://specieslink.net/
#Acesse sua chave: https://specieslink.net/aut/profile/apikeys
my_key <- "eANSwxUgXoO6tUEqIKSV" #Insira sua chave aqui

# Também podemos salvar essa informação no nosso R
# Abra seu arquivo do R environment
usethis::edit_r_environ()
# No arquivo, adicione essa linha
SPLINK_KEY = "eANSwxUgXoO6tUEqIKSV"

#Salve o arquivo, feche e reinicie o R (Session > Restart R)
# Carregue os pacotes novamente
# Teste para ver se R armazenou informações
Sys.getenv("SPLINK_KEY")

# DICA: CRIE UM OBJETO COM O NOME DA ESPÉCIE!
# Esse objeto sera usado para criar pastas e baixar as ocorrências
sp <- "Araucaria angustifolia"

#Criar diretório para salvar ocorrencias do specieslink
sp_dir <- file.path("Ocorrencias", sp)
sp_dir
dir.create(file.path(sp_dir, "SpeciesLink"), recursive = TRUE)

#Download de ocorrencias do specieslink
occ_sp <- rspecieslink2(species = sp,
                        key = Sys.getenv("SPLINK_KEY"),
                        Coordinates = "original",
                        Synonyms = "no synonyms",
                        Scope = "p", limit = 5000)
colnames(occ_sp)

#Salvar registros
fwrite(occ_sp,
       file.path(sp_dir, "SpeciesLink/Ocorrencias.gz"),
       compress = "gzip", row.names = FALSE)

# Importar novamente
occ_sp <- fread(file.path(sp_dir, "SpeciesLink/Ocorrencias.gz"))

# Espacializar e comparar com gbif
# Primeiro, precisamos forças as colunas a serem numericas
occ_sp$decimallongitude <- as.numeric(occ_sp$decimallongitude)
occ_sp$decimallatitude<- as.numeric(occ_sp$decimallatitude)
#Espacializar
occ_splink <- vect(occ_sp, geom = c(x = "decimallongitude",
                                    y = "decimallatitude"), crs = "epsg:4326")

#Plotar
mapview(occ_gbif, col.regions = "red", layer.name = "GBIF") +
  mapview(occ_splink, col.regions = "blue", layer.name = "SpeciesLink")


#### Download de registros do BIEN ####
# Botanical Information and Ecology Network Database
# https://bien.nceas.ucsb.edu/bien/

# Carregar pacote para baixar dados do BIEN
library(BIEN)
occ_bien <- BIEN_occurrence_species(species = sp,
                                    #Retornar cultivadas também?
                                    cultivated = TRUE,
                                    #Retornar informação se é nativa ou não do local:
                                    native.status = TRUE,
                                    # Retornar tipo de observação (specime ou plot)
                                    observation.type = TRUE,
                                    only.geovalid = T)
# Demora um pouco...
occ_bien %>% count(datasource)
occ_bien %>% count(dataset, sort = T)
# Mas...
occ_bien %>% select(longitude, latitude) %>% count()
# Todos os registros sem coordenadas 😭
# Vamos tentar com outra espécie
occ_bien <- BIEN_occurrence_species(species = "Paubrasilia echinata", # Pau-brasil
                                    #Retornar cultivadas também?
                                    cultivated = TRUE,
                                    #Retornar informação se é nativa ou não do local:
                                    native.status = TRUE,
                                    # Retornar tipo de observação (specime ou plot)
                                    observation.type = TRUE,
                                    only.geovalid = T)
# Espacializar
occ_pau_brasil <- vect(occ_bien, geom = c(x = "longitude", "latitude"),
                       crs = "epsg:4326")
mapview(occ_pau_brasil)

#### Download de registros do iDigBio ####
# https://www.idigbio.org/

# Carregar pacotes
library(ridigbio)

# Primeiro, vamos especificar as colunas para retornar
f <- c("basisofrecord", "canonicalname", "scientificname",
       "catalognumber", "collectioncode", "collectionid",
       "collector", "coordinateuncertainty", "country", "county",
       "stateprovince", "locality", "municipality", "datasetid",
       "datecollected", "geopoint","datasetid")

# Baixar dados
occ_digbio <- idig_search_records(rq=list(scientificname = sp,
                                          kingdom = "plantae"),
                                  fields = f)
#Remover registros sem coordenadas
occ_digbio <- occ_digbio %>% filter(!is.na(geopoint.lon), !is.na(geopoint.lat))

#Salvar
dir.create(file.path(sp_dir, "iDigBio"))
fwrite(occ_digbio, file.path(sp_dir, "iDigBio", "Ocorrencias.gz"))

#Importar de novo, se necessário
occ_digbio <- fread(file.path(sp_dir, "iDigBio", "Ocorrencias.gz"))

#Espacializar
occ_digbio2 <- vect(occ_digbio, geom = c(x = "geopoint.lon", y = "geopoint.lat"),
                    crs = "epsg:4326")
#Comparar
mapview(occ_gbif, col.regions = "red", layer.name = "GBIF") +
  mapview(occ_splink, col.regions = "blue", layer.name = "SpeciesLink") +
  mapview(occ_digbio2, col.regions = "forestgreen", layer.name = "iDigBio")


#### Integrar ocorrências de diferentes bases de dados ####

# Para unir dados de diferentes fontes (ex, GBIF + SpeciesLink + IDigBio), precisamos que os dataframes tenham as mesmas colunas e as mesmas classes.
# Problema comum com colunas com datas, que podem ser numeric, character ou Date.
# Colunas com informações de local podem ter problemas com caracteres especiais (ç, ã, ó...)

# Vamos importar os dados novamente
occ_gbif <- fread("Ocorrencias/Araucaria angustifolia/GBIF/Ocorrencias.gz")
occ_splink <- fread("Ocorrencias/Araucaria angustifolia/SpeciesLink/Ocorrencias.gz")
occ_digbio <- fread("Ocorrencias/Araucaria angustifolia/iDigBio//Ocorrencias.gz")

#Tentar unir dados
occ_list <- list(occ_gbif, occ_splink, occ_digbio) #Criar list
occ <- rbindlist(occ_list)

# Importar função que ajuda a padronizar dados
source("scripts/helpers/fix_columns.R") #Corrigir e padronizar colunas

# Essa função renomeia colunas, corrige erros com caracteres especiais e muda classe de colunas (numeric e character) se necessário.

#Primeiro, vamos identificar colunas que devem ser numericas...
nc <- c("decimalLongitude", "decimalLatitude",
        "coordinateUncertaintyInMeters", "elevation", "year")

#Depois, identificar colunas para checar encoding (problemas com caracteres especiais)
c_encod <- c("collectionCode", "catalogNumber",
             "country", "stateProvince", "municipality",
             "locality", "eventDate", "recordedBy", "identifiedBy",
             "basisOfRecord", "datasetName")

#Agora vem a parte mais chata e trabalhosa: criar um dataframe com as colunas de cada data.frame com registros de cada base de dados
# Metadados de GBIF
gbif_metadata <- data.frame(scientificName = "acceptedScientificName", #Nome científico
                            occurrenceID = "gbifID", #ID da ocorrência
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
                            key = "speciesKey") #Chave da espécie no GBIF
# Esse dataframe será usado pela função fix_columns para padronizar os dados do GBIF
#Corrigir colunas
gbif_fixed <- fix_columns(data = occ_gbif, #Tabela com ocorrencias
                          numeric_columns = nc, #Colunas que devem ser numericas
                          check_encoding = c_encod, #Colunas pare checar encoding
                          metadata = gbif_metadata, #Metadados com colunas
                          data_source = "gbif") #Fonte dos dados
#Vamos comparar os nomes das colunas
colnames(gbif_fixed)
colnames(occ_gbif)

# Padronizar species link #
#Criar coluna com occurrenceID
occ_splink$ocurrenceID <- 1:nrow(occ_splink)
# Criar dataframe com metadados identificando as colunas
splink_metadata <- data.frame(scientificName = "scientificname", #Nome científico
                              occurrenceID = "ocurrenceID", #ID da ocorrência
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

#Corrigir colunas
splink_fixed <- fix_columns(data = occ_splink, #Tabela com ocorrencias
                            numeric_columns = nc, #Colunas que devem ser numericas
                            check_encoding = c_encod, #Colunas pare checar encoding
                            metadata = splink_metadata, #Metadados com colunas
                            data_source = "splink") #Fonte dos dados


# Padronizar iDigBio #
#Criar coluna com occurrenceID
occ_digbio$ocurrenceID <- 1:nrow(occ_digbio)
# Extrair ano
occ_digbio$year <- str_sub(occ_digbio$datecollected, 1, 4) %>%
  as.numeric()
# Criar metadados
db_metadata <- data.frame(scientificName = "scientificname",
                          occurrenceID = "ocurrenceID",
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
# Corrigir colunas
db_fixed <- fix_columns(data = occ_digbio,
                        metadata = db_metadata,
                        data_source = "digbio")

# Unir dados
occ_final <- rbindlist(list(gbif_fixed, splink_fixed, db_fixed))

# Vamos criar uma coluna chamada ID, para identificar os registros
occ_final <- occ_final %>%
  mutate(ID = 1:nrow(occ_final),
         .before = 1) #Colocar como primeira coluna
head(occ_final)

# Salvar dados
fwrite(occ_final, file.path(sp_dir, "1-Ocorrencias.gz"))

# Espacializar
pts_final <- vect(occ_final, geom = c(x = "decimalLongitude",
                                       y = "decimalLatitude"),
                   crs = "epsg:4326")
# Plot interativo
mapview(pts_final,
        zcol = "data_source", # Uma cor para cada data_source
        burst = TRUE) #Tratar cada zcol como uma camada individual

