#### Autocorrelação espacial ####
# A distribuição de um fenômeno no espaço é aleatória, ou existe um padrão?
# Agrupamento: locais com valores semelhantes estão próximos uns dos outros
# Dispersão: locais com valores semelhanates se repelem
# Aleatório: sem padrão.

# Primeira lei da geografia:
# Tudo está relacionado a tudo o mais, mas coisas próximas estão mais relacionadas do que coisas distantes

# O Índice de Moran (I de Moran) é um índice que compara a covariância espacial (o quanto os valores de "vizinhos" variam juntos) e a variância total dos dados.
# Valores positivos indicam agrupamento (autocorrelação espacial positiva)
# Valores negativos indicam dispersão (autocorrelação espacial negativa)
# Valores próximos de 0 indiam aleatoriedade (sem autocorrelação espacial)

# Uma das utilidades do I de Moran é reduzir vieses de amostragem de ocorrências de espécies


# Carregar pacotes
library(terra)
library(dplyr)
library(mapview)
library(kde)
library(spatialEco)
library(pals)
library(data.table)
library(sf)
library(spThin)
library(moranfast)
library(pbapply)

# Importar função
source("scripts/helpers/filter_geo_moran.R")

# Importar registros de ocorrência
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros
occ <- fread(file.path(sp_dir, "4-Ocorrencias_sem_outliers.gz"))

# Espacializar e plotar
pts <- vect(occ, geom = c(x = "decimalLongitude", y = "decimalLatitude"),
            crs = "epsg:4326")
mapview(pts)

# Vamos fazer um mapa de calor
# Converter pts para sf
pts_sf <- st_as_sf(pts)
mapa_calor <- sp.kde(x = pts_sf, #Pontos (em formato sf)
                     res = 0.045, # Resolução do raster: ~5km
                     standardize = T) #Escalar de 0 a 1
mapview(mapa_calor, col.regions = rev(pals::brewer.spectral(7)))

# Será que a Araucariá gosta tanto assim de Curitiba? Ou é onde teve mais coletas?
# Para amenizar esse viés de coleta, podemos filtrar os pontos por distância, removendo pontos agrupados
# Por exemplo, podemos deixar apenas um ponto a cada 10km

# Para isso, vamos usar o pacote spThin
occ_10km <- thin(loc.data = occ,
                 lat.col = "decimalLatitude", long.col = "decimalLongitude",
                 spec.col = "species", thin.par = 10, reps = 1,
                 locs.thinned.list.return = TRUE,
                 write.files = FALSE)
occ_10km <- occ_10km[[1]]
nrow(occ)
nrow(occ_10km)

# Vamos analisar o mapa de calor
pts_10km <- vect(occ_10km, geom = c(x = "Longitude", y = "Latitude"),
                 crs = "epsg:4326")
pts_10km_sf <- st_as_sf(pts_10km)
mapa_calor_10km <- sp.kde(x = pts_10km_sf, #Pontos (em formato sf)
                          res = 0.045, # Resolução do raster: ~5km
                          standardize = T) #Escalar de 0 a 1
mapview(mapa_calor_10km, col.regions = rev(pals::brewer.spectral(7)))
# Melhorou um pouco!

# Vamos analisar a autocorrelação espacial desses dois conjuntos de pontos (original e filtrado)
# Para isso, vamos usar o pacote moranfast

# Primeiro, vamos extrair os valores das variáveis para os pontos
#Importar
variaveis <- rast("Variaveis/Variaveis_neotropicos.tif")
#Importar nomes das variaveis para manter
var_to_keep <- readRDS("Variaveis/Variaveis_para_manter.rds")
# Selecionar variaveis
v <- variaveis[[var_to_keep]]

# Extrair valores para os pontos
occ_var <- terra::extract(v, pts, xy = TRUE, na.rm = TRUE) #Originais
occ_10km_var <- terra::extract(v, pts_10km, xy = TRUE, na.rm = TRUE) #Fltrados com distancia de 10km

# Calcular I de Moran com moranfast para variável bio_6
imoran_original <- moranfast(occ_var$bio_6, occ_var$x, occ_var$y)
imoran_original
imoran_10km <- moranfast(occ_10km_var$bio_6, occ_10km_var$x, occ_10km_var$y)
imoran_10km

# Caso não consiga instalar o moranfast...
# Calcular I de Moran com ape para variável bio_6
# Obter matriz de distancia
dist_original <- occ_var %>% select(x,y) %>% as.matrix() %>% dist()
dist_original <- as.matrix(dist_original)
dist_original <-  1/dist_original
diag(dist_original) <- 0
dist_original[is.infinite(dist_original)] <- 0 #Remover valores finitos
imoran_original <- ape::Moran.I(occ_var$bio_6, dist_original,
                                scaled = T, na.rm = TRUE)
imoran_original

# Com dados filtrados para 10km
dist_10k <- occ_10km_var %>% select(x,y) %>% as.matrix() %>% dist()
dist_10k <- as.matrix(dist_10k)
dist_10k <-  1/dist_10k
diag(dist_10k) <- 0
dist_10k[is.infinite(dist_10k)] <- 0 #Remover valores finitos
imoran_10k <- ape::Moran.I(occ_10km_var$bio_6, dist_10k,
                           scaled = T, na.rm = TRUE)
imoran_10k

# A questão é: qual a distância ideal para minimizar o efeito da autocorrelação espacial?
# 5km? 10km? 20km? 50km?
# Uma maneira de decidir isso é testar várias distâncias
# E selecionar aquela que oferece um balanço entre:
  # Diminui autocorrelação espacial (mas não ao ponto de eliminar totalmente)
  # Mantem maior numero de pontos possível

# Vamos usar a função filter_geo_moran() para testar várias distâncias
# Se não conseguiu instalar o pacote moranfast, utilize o filter_geo_moran_ape()
moran_tests <- filter_geo_moran(occ = occ,
                                species = "species",
                                long = "decimalLongitude",
                                lat = "decimalLatitude",
                                d = seq(0, 30, 2), #de 0 a 30km, de 2 em 2
                                variables = v)
moran_tests$Distance
View(moran_tests$imoran)
nrow(moran_tests$occ)
View(moran_tests$occ)

#Vamos plotar os pontos e ver o mapa de calor com os pontos filtrados de acordo com o I de Moran
pts_moran <- vect(moran_tests$occ, geom = c(x = "x", y = "y"),
                  crs = "epsg:4326")
pts_moran_sf <- st_as_sf(pts_moran)
mapa_calor_moran <- sp.kde(x = pts_moran_sf, #Pontos (em formato sf)
                          res = 0.045, # Resolução do raster: ~5km
                          standardize = T) #Escalar de 0 a 1
mapview(mapa_calor_moran, col.regions = rev(pals::brewer.spectral(7))) +
  mapview(pts_moran, cex = 3)

# Lembrando: isso não é um modelo de distribuição ou de nicho!

# Filtrar pontos é importante para que consigamos capturar a preferência da espécie, e não a preferência de quem estuda a espécie (e coleta em lugares mais acessíveis e conhecidos)

# Vamos salvar essas ocorrencias filtradas (thinned)
fwrite(moran_tests$occ,
       file.path(sp_dir, "5-Ocorrencias_baixa_autocorrelacao_espacial.gz"))
