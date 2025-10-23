#### Funções ####
# 1. Baixar ocorrências do speciesLink: documentar e melhorar

# 2. Baixar ocorrências do gbif? - Criar do zero
# 2. Baixar ocorrências de outras bases de dados? (BIEN, digbio) - Criar do zero

# 3. Padronizar colunas de occorrencias de diferentes bases de dados: documentar e melhorar
  # Padroniza classe de colunas (coordenadas como numeric, data como character)
  # Remove caracteres especiais
  # Extrai ano de data (2024-20-01 -> 2024)

# 4. Unir dados de diferentes bases de dados (checar data.frames antes de unir) - Criar do zero

# 5. Remover individuos cultivados - Criar do zero inspirado em:
?plantR::getCult

# Fazer algo parecido com fósseis e registros do iNaturalist sem research-grade?


# 6. Remover duplicatas baseado em alguma coluna (ex: ano, fonte) - Criar do zero

# 7. Padronizar nomes de países e Estados - Criar quase do zero (usar padrão do     rnaturalearth::countries110??)

# 8. Checar se coordenadas caem nos países e Estados descritos nos metadados: documentar e melhorar
  # Testar coordenadas com sinais invertidos e/ou trocados

# 9. Filtrar registros (thinning) usando autocorrelação espacial I de Moran: documentar e melhorar (desafio: 'moranfast', não está no CRAN. Talvez melhorar função em C++ do moranfast?)

# 10. PRINCIPAL FUNÇÃO DO PACOTE!
# Usar informações de especialistas para filtrar dados. Implementar:
#
  # Flora do Brasil - pacote florabr
  # Fauna do Brasil - pacote faunabr
  # World Checklist of Vascular Plants (WCVP) - pacote rWCVP
  # BIEN - pacote BIEN (função BIEN_ranges_species)
  # Fishbase - pacote rfishbase
  # Artigo "Native range estimates for red-listed vascular plants" - USA MESMO QUE WCVP
  # IUCN - Como acessa dados sem baixar todas as espécies????????
  # Birdlife - ?????????
  # Outras bases de dados?
  # expowo: An R package for mining global plant diversity  and distribution data - mesmo que wcvp

# Checar espécies disponíveis em cada base

# Tentar paralelizar

# Implementar funções do CoordinateCleaner no pacote??

# 11. Funções de sumarização: plots de mapas, estatísticas, etc: ggplot2 e mapview

# Outras sugestões de funções.
# Plot range - colorir por trait


# Sugestão de workflow:
# Dado de entrada: ocorrências unidas (Passo 4)
# Funções vão retornando listas com ocorrências filtradas e removidas (ou index das ocorrências?)
# Essas listas são objetos com uma classe específica
# Funções de plot baseadas nos elementos dessa lista (ex: mapa com pontos e legenda do porque foram removidos)

# Desafios:
# - Usar o menor número possível de pacotes! (todos do CRAN)
    # Se for usar uma unica função de um pacote, "copiar/plagiar" função.
# - Pacote não pode ter mais de 5Mb
    # Funções para baixar informações de bases de dados. Ex: 'florabr' baixa informações mais recentes da Flora e Funga do Brasil

# Sugestões de nomes do pacote:
# RUhere ou RUthere (inspirado em R U MINE)

# Outras sugestões?

# https://onlinelibrary.wiley.com/doi/full/10.1111/geb.13847


#### Função para corrigir casa decimal ####
# Colocar dentro de check_countries?
