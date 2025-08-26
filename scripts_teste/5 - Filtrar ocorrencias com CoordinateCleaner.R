#### Finalizar filtragem com CoordinateCleaner ####
# Vamos finalizar a filtragem dos pontos usando o pacote CoordinateCleaner:
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13152

# "CoordinateCleaner compares the coordinates of occurrence records to reference
# databases of country and province centroids, country capitals, urban areas,
# known natural ranges and tests for plain zeros, equal longitude/latitude,
# coordinates at sea, country borders and outliers in collection year"


#Carregar pacotes
library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(CoordinateCleaner) #Filtrar pontos

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros
occ <- fread(file.path(sp_dir, "2-Ocorrencias_especialistas.gz"),
             data.table = FALSE)
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts,
        zcol = "year") #Coluna usada para colorir

# Vamos usar algumas informações dos metadados para remover pontos duvidosos #
# Vamos remover os pontos sem ano muito antigos (Antes de 1980)
antigos <- which(occ$year < 1980 #Identificar quais pontos foram registradoa antes de 1980
                      | is.na(occ$year)) #Ou que estão sem info de ano
antigos #index (numero da linha) de registros antigos
mapview(pts[antigos,], zcol = "year")

# Vamos salvar os IDS dos registros removidos
arquivo_original <- file.path(sp_dir, "2-Ocorrencias_especialistas.gz")
id_antigos <- occ[antigos, "ID"]
removidos <- list("arquivo_original" = arquivo_original,
                  "removidos_antigos_1980" = id_antigos)

#Vamos remover esses pontos do nosso dataframe
occ <- occ[-antigos, ]
pts <- pts[-antigos, ]

# As colunsa occurrenceRemarks e locality também oferecem algumas informações uteis
# Por exemplo, de individuos cultivados/plantados
cultivados_remarks <- grep("cultiv|plantad", occ$occurrenceRemarks) #Em occurrenceRemarks
cultivados_locality <- grep("cultiv|plantad", occ$locality) #Em locality
# Unir index de cultivados de acordo com occurrenceRemarks e locality
cultivados <- c(cultivados_remarks, cultivados_locality) %>% unique()
#Ver informações
occ$occurrenceRemarks[cultivados] %>% unique() #Ver informações
occ$locality[cultivados_locality] %>% unique()
# Ver mapa
mapview(pts[cultivados, ], label = "occurrenceRemarks")

# Vamos adicionar os IDs desses registros removidos
removidos[["cultivados"]] <- occ$ID[cultivados]
removidos

# Vamos remover cultivados
occ <- occ[-cultivados, ]
pts <- pts[-cultivados, ]

# Alguns individuos também podem estar em herbários ou universidades
instituicoes_biod_remarks <- grep("herb|Herb|univ|Univ", occ$occurrenceRemarks)
instituicoes_biod_locality <- grep("herb|Herb|univ|Univ", occ$locality)
# Unir dados
instituicoes_biod <- c(instituicoes_biod_remarks,
                       instituicoes_biod_locality) %>% unique()
# Ver informações
occ$occurrenceRemarks[instituicoes_biod_remarks] %>% unique()
occ$occurrenceRemarks[instituicoes_biod_locality] %>% unique()
#Plotar
mapview(pts[instituicoes_biod, ], label = "occurrenceRemarks")

# Vamos adicionar os IDs desses registros removidos
removidos[["herbarios_universidades"]] <- occ$ID[instituicoes_biod]
removidos


# Remover pontos em instituicoes
occ <- occ[-instituicoes_biod, ]
pts <- pts[-instituicoes_biod, ]
mapview(pts)

# E os pontos do inaturalist?
inat <- grep("iNat", occ$datasetName)
mapview(pts[inat, ])
# Deixar ou não?

# O pacote CoordinateCleaner oferece algumas opções para filtrar registros automaticamente
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13152

# Pontos em centróides de países
cap <- cc_cap(occ, lon = "decimalLongitude", lat = "decimalLatitude",
              species = "scientificName",
              buffer = 1000, #Buffer do centroide em metros
              value = "flagged")
sum(!cap) #Numero de registros sinalizados

#Função para salvar registros se houver sinalizações
if(sum(!cap) > 0){ #Se algum registro for sinalizado...
  # Salvar index removidos
  removidos[["centroides_paises"]] <- occ$ID[!cap]

  #Manter apenas ocorrências que passaram no teste
  occ <- occ[cap,]
  }

# Pontos em centróides de Estados do Brasil
br <- geobr::read_state() %>% vect() #Obter mapa do brasil
br_cen <- terra::centroids(br) #Obter centroides de estados
br_cen_b <- buffer(br_cen, width = 1 * 1000) #Adicionar buffer de 1 km
mapview(br_cen_b)
#Checar se algum ponto cai dentro do centróide
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
cen <- !is.related(pts, br_cen_b, "intersects") #Se não cai dentro, é TRUE (passou no teste)
sum(!cen) #Numero de registros sinalizados

#Função para salvar registros se houver sinalizações
if(sum(!cen) > 0){ #Se algum registro for sinalizado...
  # Salvar index removidos
  removidos[["centroides_estados"]] <- occ$ID[!cen]
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[cen,]
}

#Registros com longitude e latitude identicos
equ <- cc_equ(occ, lon = "decimalLongitude",
              lat = "decimalLatitude",
              test = "absolute",
              value = "flagged")
sum(!equ) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!equ) > 0){ #Se algum registro for sinalizado...
  # Salvar index removidos
  removidos[["long_lat_iguais"]] <- occ$ID[!equ]
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[equ,]
}

#Registros fora do sistema de coordenadas
# lat >90, lat <-90, lon > 180 and lon < -180
val <- cc_val(occ, lon = "decimalLongitude",
              lat = "decimalLatitude",
              value = "flagged")
sum(!val) #Numero de registros sinalizados


#Registros em coordenadas 0-0 (cairia no mar)
zero <- cc_zero(occ, lon = "decimalLongitude",
                lat = "decimalLatitude",
                buffer = 0.1, #Buffer in degrees
                value = "flagged")
sum(!zero) #Numero de registros sinalizados

#Pontos na sede do GBIF em Copenhagen
gbif <- cc_gbif(occ, lon = "decimalLongitude",
                lat = "decimalLatitude",
                species = "scientificName",
                buffer = 1000, #Buffer em metros
                value = "flagged")
sum(!gbif) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!gbif) > 0){ #Se algum registro for sinalizado...
  # Salvar index removidos
  removidos[["sede_gbif"]] <- occ$ID[!gbif]
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[gbif,]
}

#Pontos em instituições de biodiversidade (jardins botanicos, herbarios, museus, etc)
#Antes, vamos ver a localização dessas instituições
bio_inst <- vect(institutions[!is.na(institutions$decimalLongitude) & #Mapa de referencia do CoordinateCleaner
                                !is.na(institutions$decimalLatitude), ],
                 geom = c(x = "decimalLongitude", y = "decimalLatitude"),
                 crs = "+proj=longlat +datum=WGS84 +no_defs")
mapview(bio_inst)
#Testar se pontos caem nas instituições
inst <- cc_inst(occ, lon = "decimalLongitude",
                lat = "decimalLatitude",
                species = "scientificName",
                buffer = 1000, #A 1km de distancia
                value = "flagged")
sum(!inst) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!inst) > 0){ #Se algum registro for sinalizado...
  # Salvar index removidos
  removidos[["instituicoes_biodiversidade"]] <- occ$ID[!inst]
  #Remover registros
  occ_inst_removed <- occ[!inst,]
  #Ver mapa
  pts_inst_removed <- vect(occ_inst_removed, #Converte pontos para spatvector
                           geom = c(x = "decimalLongitude",
                                    y = "decimalLatitude"),
                           crs = "+init=epsg:4326")
  mapview(bio_inst) + mapview(pts_inst_removed, col.regions = "yellow")
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[inst,]
}

### Remover duplicatas (apenas coordenadas)

#Vamos escolher a ordem de preferência das bases de dados
unique(occ$data_source)
# Ex: mesmo registro no gbif e specieslink, manter do gbif
preferred_order <- c("gbif", "splink", "digbio")
####Filtrar duplicatas
occ_unique <- occ %>%
  group_by(scientificName, decimalLongitude, decimalLatitude) %>% #Identificar valores duplicados
  arrange(match(data_source, preferred_order)) %>% #Definir ordem de preferencia de data_source
  filter(row_number() == 1) %>% #Remover duplicados
  ungroup()

#Obter valores duplicados removidos
dup_id <- setdiff(occ$ID, occ_unique$ID)
dup_id
# Salvar index removidos por serem duplicados
removidos[["duplicados"]] <- dup_id

# Ver duplicados
occ_dup <- occ %>% filter(ID %in% dup_id)
pts_dup <- vect(occ_dup, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                                  y = "decimalLatitude"), crs = "+init=epsg:4326")
#Checar pontos
mapview(pts) + mapview(pts_dup, #Converte pontos para spatvector
                       zcol = "data_source", #Coluna usada para colorir
                       col.regions = c("red", "forestgreen", "yellow"), #Cores
                       burst = TRUE) #Filtrar por valor da coluna
# Atualizar occ
occ <- occ_unique

# Filtrar por outliers geográficos
# Cuidado: outliers são mesmo outliers? Ou é apenas uma distribuição disjunta?
# If “quantile”: a boxplot method is used and records are flagged as outliers
# if their mean distance to all other records of the same species is larger than
# mltpl * the interquartile range of the mean distance of all records of this species. I
?cc_outl
outl <- cc_outl(x = occ,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "scientificName",
  method = "quantile", #Vamos usar o método de quantil
  mltpl = 5, #Multiplicador do método quartil
  value = "flagged",
  verbose = TRUE,
  min_occs = 7)
sum(!outl)
#Ver pontos marcados como outliers
pts_outl <- vect(occ, #Converte pontos para spatvector
                 geom = c(x = "decimalLongitude",
                          y = "decimalLatitude"),
                         crs = "+init=epsg:4326")
#Adicionar info de outliers
pts_outl$outlier <- outl
mapview(pts_outl, zcol = "outlier")

# Ponto é mesmo um outlier?
# https://www.inaturalist.org/observations/252623928

# Por enquanto, vamos apenas deixar indicado que é um outlier geográfico
occ$outlier <- outl #Criar coluna
# Lembre que TRUE = não é outlier e FALSE = Outlier (não passou no teste)

# Podemos também ver se é outlier no espaço ambiental...
# Vamos ver mais a frente!

#SALVAR CHECKPOINT
fwrite(occ,
       file.path(sp_dir, "3-Ocorrencias_CoordinateCleaner.gz"),
       compress = "gzip", row.names = FALSE)
# Salvar também os pontos removidos
saveRDS(removidos,
        file.path(sp_dir, "Removidos", "3-removidos_CoordinateCleaner.rds"))



#Ver mapa final
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
pts$basisOfRecord[pts$basisOfRecord == ""] <- NA #Substituir valores vazios por NA
mapview(pts, #Converte pontos para spatvector
        zcol = "basisOfRecord", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna
