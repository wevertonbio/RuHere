#### Usando informações de especialistas para remover registros duvidosos ####

# Carregar pacotes
library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(florabr) #Acesso a Flora do Brasil
library(rWCVPdata) #Acesso a base do WCVP (World Checklist of Vascular Plants)
library(stringr) #Manipulação de texto

#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros
occ <- fread(file.path(sp_dir, "1-Ocorrencias.gz"))

#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#### FILTRAR REGISTROS USANDO INFORMAÇÕES DA FLORA DO BRASIL ####

# Carregar dados da Flora do Brasil
# Dados baixados no script anterior
fbr <- load_florabr(data_dir = "florabr")

# Antes de filtrar, vamos obter algumas informações da espécie
sp_info <- subset_species(data = fbr, species = sp)
# Espécie é endemica do Brasil?
sp_info$endemism
# Estados com ocorrências confirmadas (segundo especialistas)
sp_info$states
# Biomas com ocorrências confirmadas (segundo especialistas)
sp_info$biome

# Obter poligonos dos estados e biomas com ocorrências confirmadas
sp_pol <- get_spat_occ(data = fbr, species = sp)
plot(sp_pol$`Araucaria angustifolia`$states)
plot(sp_pol$`Araucaria angustifolia`$biomes)
plot(sp_pol$`Araucaria angustifolia`$states_biomes)

#Vamos usar essas informações para filtrar nossas ocorrências
# Poderíamos fazer isso manualmente usando a função intersect...
occ_in_states <- is.related(pts,
                            sp_pol$`Araucaria angustifolia`$states,
                            "intersects")
table(occ_in_states)

# Porém teríamos que fazer isso manualmente por estado e depois por bioma
# Além disso, só faz sentido testar isso para os pontos que caem no Brasil

#O pacote florabr possui uma função para filtrar pontos de ocorrência por Estado, Bioma e endemismo no Brasil
?filter_florabr

# O pacote florabr funciona com nomes binomiais, sem ser o nome completo
# Vamos checar como estão os nomes das espécies na tabela de ocorrências:
unique(occ$scientificName)

# Vamos usar a função get_binomial para criar uma nova coluna com o nome da espécie
occ <- occ %>%
  mutate(species = get_binomial(scientificName, #Criar coluna species
                                include_subspecies = FALSE, #Não incluir subespecie
                                include_variety = FALSE), #Não incluir variedade
         .before = scientificName) #Posicionar coluna antes dessa coluna
# Checar nomes
unique(occ$species)

# Alguns dos nomes estão em minuscula.
# Vamos corrigir isso usando a função str_to_sentence() do pacote stringr
?str_to_sentence
occ$species <- str_to_sentence(occ$species)
# Checar nomes
unique(occ$species)

# Sinalizar pontos potencialmente incorretos
occ_florabr <- filter_florabr(data = fbr,
                              occ = occ,
                              species = "species", #Coluna com nome da espécie
                              long = "decimalLongitude", #Nome da coluna com long
                              lat = "decimalLatitude", #Nome da coluna com lat
                              by_state = TRUE, #Filtrar por estado?
                              buffer_state = 20, #Buffer ao redor de estados
                              by_biome = TRUE, #Filtrar por bioma?
                              buffer_biome = 20, #Buffer ao redor de biomas
                              by_endemism = TRUE, #Checar endemismo no Brasil?
                              buffer_brazil = 20, #Buffer ao redor do brasil
                              #Retornar registros sinalizados e filtrados
                              value = "flag&clean")

# A função retorna uma lista com dois objetos:
names(occ_florabr)
# flagged: registros com colunas sinalizando registros potencialmente incorretos
# Registros potencialmente incorretos devido ao endemismo no Brasil
occ_florabr$flagged %>% count(inside_br)

# Registros potencialmente errados  por caírem em estados diferentes
occ_florabr$flagged %>% count(inside_state)
# NAs indicam registros fora do Brasil

# Registros potencialmente errados por caírem em biomas diferentes
occ_florabr$flagged %>% count(inside_biome)
# NAs indicam registros fora do Brasil

#Vamos plotar esses registros sinalizados
flagged <- vect(occ_florabr$flagged, geom = c(x = "decimalLongitude",
                                              y = "decimalLatitude"),
                crs = "epsg:4326")
#Plot interativo
mapview(sp_pol$`Araucaria angustifolia`$states,
        layer.name = "Estados") + #Estados com ocorrencia
  mapview(sp_pol$`Araucaria angustifolia`$biomes,
          layer.name = "Biomas") + #Biomas com ocorrencia
  mapview(flagged, zcol = "filters_ok",
          col.regions = c("firebrick", "forestgreen"),
          cex = 3, layer.name = "Ocorrencias")

# O objeto "cleaned" retorna os registros filtrados, removendo os potencialmente incorretos
nrow(occ_florabr$flagged)
nrow(occ_florabr$cleaned)

# Vamos usar esses registros como nosso novo conjunto de ocorrências
new_occ <- occ_florabr$cleaned

# Dica: salve os registros removidos. Talvez, você precise consultá-los no futuro
# Ao invés de salvar a tabela (que pode ocupar muito espaço), salve o ID do registro removido
# Por exemplo, vamos criar uma pasta chamada "Removidos"
dir.create(file.path(sp_dir, "Removidos"))
# Agora, vamos criar uma lista, com o caminho do arquivo original e os indices removidos
arquivo_original <- file.path(sp_dir, "1-Ocorrencias.gz")
id_removidos_flora <- occ_florabr$flagged %>%
  filter(!filters_ok) %>% #selecionar apenas registros não ok (! indica falso)
  pull(ID) #Pegar valores da coluna ID
# Salvar numa lista
removidos_flora <- list("arquivo_original" = arquivo_original,
                        "removidos_florabr" = id_removidos_flora)

# Depois, se quiser ver quais foram removidos, basta usar essa informação
df_original <- fread(removidos_flora$arquivo_original)
df_removidos <- df_original %>%
  filter(ID %in% removidos_flora$removidos_florabr)

#Salvar lista com arquivo original e index removidos
saveRDS(removidos_flora, file.path(sp_dir, "Removidos", "2-removidos_flora"))

# E os registros fora do Brasil?

# Podemos utilizar informações do WCVP!

#### FILTRAR REGISTROS USANDO INFORMAÇÕES DO WCVP ####
# World Checklist of Vascular Plants
# https://matildabrown.github.io/rWCVP/articles/coordinate-cleaning.html

# Importar função para sinalizar registros com WCVP
source("scripts/helpers/filter_wcvp.R")
# Unir os dados dos nomes das espécies e distribuição em um unico dataframe
# Para isso, vamos usar a função customizada prepare_wcvp_data()
# wcvp_names <- rWCVPdata::wcvp_names #Nome das espécies
# wcvp_dist <- rWCVPdata::wcvp_distributions #Distribuição
# wcvp_data <- prepare_wcvp_data(wcp_names = wcvp_names,
#                                wcp_dist = wcvp_dist,
#                                wcp_map = wcvp_map)
# # Salvar registros
# fwrite(wcvp_data$wcp_data, "scripts/helpers/wcvp_data.gz")

# Importar dados tratados de wcvp (gerados com o script acima)
wcvp_data <- fread("scripts/helpers/wcvp_data.gz")

# Obter mapa base utilizado como referencia pelo wcvp
wcvp_map <- rWCVPdata::wgsrpd3 #Mapa referente a distribuição
# Converter para spatvector
wcvp_map <- vect(wcvp_map[,c("LEVEL3_NAM", "LEVEL3_COD")])
plot(wcvp_map)

#Vamos usar a função customizada filter_wcvp() para filtrar pontos fora dessas regiões:
occ_wcvp <- filter_wcvp(occ = new_occ, #Tabela com ocorrencias
                        species = "species", #Nome da coluna com espécies
                        x = "decimalLongitude", #Nome da coluna com longitude
                        y = "decimalLatitude", #Nome da coluna com latitude
                        wcvp_data = wcvp_data, # Dados preparados de wcvp
                        wcvp_map = wcvp_map, # Mapa de wcvp
                        buffer = 20, #Buffer de tolerância ao redor das regiões (em km)
                        return_map = TRUE) #Retornar mapa de distribuição de cada espécie?

#Checar pontos sinalizados
pts_wcvp <- vect(occ_wcvp[[sp]]$flagged,
                 geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
# Plotar resultados
mapview(pts_wcvp, zcol = "natural_range_wcvp",
        col.regions = c("firebrick", "forestgreen"),
        layer.name = "Ocorrências",  cex = 3) +
  # Plotar poligono de distribuição natural
  mapview(occ_wcvp[[sp]]$map, layer.name = "Natural range")

#Remover registros fora de distribuição natural
occ_wcvp_filtered <- occ_wcvp$`Araucaria angustifolia`$flagged %>%
  filter(natural_range_wcvp == TRUE)

nrow(occ) #Numero de registros obtidos do GBIF
nrow(new_occ) #Numero de registros após filtrar com florabr
nrow(occ_wcvp_filtered) #Numero de registros após filtrar com wcvp


# Poderíamos implementar o mesmo procedimento com poligonos de outras fontes, como da IUCN, usando a função is.related() para identificar pontos que caem fora do polígono

# Salvar registros
fwrite(occ_wcvp_filtered,
       file.path(sp_dir, "2-Ocorrencias_especialistas.gz"))

# Vamos adicionar os IDs removidos pelo WCVP ao removidos_flora
id_removidos_wcvp <- occ_wcvp$`Araucaria angustifolia`$flagged %>%
  filter(natural_range_wcvp == FALSE) %>% pull(ID)
id_removidos_wcvp

removidos_flora[["removidos_WCVP"]] <- id_removidos_wcvp
removidos_flora

#Salvar lista com arquivo original e index removidos
saveRDS(removidos_flora, file.path(sp_dir, "Removidos", "2-removidos_flora"))
