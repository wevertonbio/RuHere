#' Title
#'
#' @param data_dir
#' @param species
#' @param datasets
#' @param return_distribution
#'
#' @returns
#' @export
#'
#' @examples
available_datasets <- function(data_dir, species, datasets = "all",
                               return_distribution = FALSE){

  # Check datasets available
  if(datasets == "all"){
  l <- list.dirs(data_dir, recursive = FALSE, full.names = FALSE)
  l <- intersect(l,
                 c("florabr", "wcvp", "iucn", "bien", "faunabr", "fishbase"))
  } else {
    l <- datasets
  }

  # Import florabr
  if("florabr" %in% l){
    flora <- florabr::load_florabr(data_dir = file.path(data_dir, "florabr"),
                                   verbose = FALSE)}

  if("faunabr" %in% l){
    fauna <- faunabr::load_faunabr(data_dir = file.path(data_dir, "faunabr"),
                                   verbose = FALSE)}

  if("wcvp" %in% l){
    wcvp <- data.table::fread(file.path(data_dir = file.path(
      data_dir, "wcvp", "wcvp.gz")))
  }

  if("iucn" %in% l){
    iucn <- data.table::fread(file.path(data_dir = file.path(
      data_dir, "iucn", "iucn_distribution.gz")))
  }

  # Get map
  if("iucn" %in% l | "wcvp" %in% l){
    if("iucn" %in% l & "wcvp" %in% l)
      m <- terra::vect(file.path(data_dir, "iucn/wgsrpd.gpkg"))

    if("iucn" %in% l & !("wcvp" %in% l))
      m <- terra::vect(file.path(data_dir, "iucn/wgsrpd.gpkg"))

    if("wcvp" %in% l & !("iucn" %in% l))
      m <- terra::vect(file.path(data_dir, "wcvp/wgsrpd.gpkg"))

    # Get dataframe to filter levels
    dm <- terra::as.data.frame(m)
  }

  #
  # if("fishbase" %in% l){
  #   fishbase <- data.table::fread(file.path(data_dir = file.path(
  #     data_dir, "fishbase", "fb_species_list.gz")))
  # }
  #

  r_species <- lapply(species, function(x){
    # Check if species x is in dataset
    r <- lapply(l, function(i){

      if(i == "bien"){
        return(file.exists(file.path(data_dir, paste0("bien/", x, ".gpkg"))))
      }

      if(i == "florabr"){
        return(x %in% flora$species)
      }

      if(i == "faunabr"){
        return(x %in% fauna$species)
      }

      if(i == "wcvp"){
        return(x %in% wcvp$species)
      }

      if(i == "iucn"){
        return(x %in% iucn$species)
      }

      if(i == "fishbase"){
        return(FALSE) #Precisa testar
      }
      })
    names(r) <- l
    # Which databases available for species x
    avx <- names(r[unlist(r)])
    dr <- data.frame(species = x,
                     datasets = paste(avx, collapse = ";"))
    return(dr)
  }) #End of r_species

  # Dataset available for each species
  r_species <- data.table::rbindlist(r_species)

  # If return distribution...
  if(return_distribution){
    d_species <- lapply(species, function(x){
      #Datasets available
      avx <- r_species$datasets[r_species == x] %>%
        strsplit(";") %>% unlist()
      if(length(avx) > 0){
        spt <- lapply(avx, function(y){

          if(y == "bien"){
            return(vect(file.path(data_dir, paste0("bien/", x, ".gpkg"))))
          }

          if(y == "florabr"){
            spt_flora <- florabr::get_spat_occ(data = flora, species = x,
                                               verbose = FALSE)[[1]]
            return(spt_flora)
          }

          if(y == "faunabr"){
            spt_fauna <- faunabr::fauna_spat_occ(data = fauna, species = x,
                                                 verbose = FALSE)[[1]]
            return(spt_fauna)
          }

          if(y == "wcvp"){
            w_x <- wcvp[wcvp$species == x]
            w_x <- m[m$Level3_cod %in% w_x$LEVEL3_COD,]
            return(w_x)
          }

          if(y == "iucn"){
            w_x <- iucn[iucn$species == x]
            # Get regions of occurrence (by level)
            country_level <- w_x$code[nchar(w_x$code) == 2]
            level3 <- w_x$code[nchar(w_x$code) == 3]
            countries_level3 <- dm$ISO_Code[dm$Level3_cod %in% level3]
            level4 <- w_x$code[nchar(w_x$code) > 3]
            countries_level4 <- dm$ISO_Code[dm$Level4_cod %in% level4]

            # Filter levels
            # Remove countries already in levels 3 or 4
            country_level[country_level %in% countries_level3 |
                            country_level %in% countries_level4] <- NA
            country_level <- na.omit(country_level)

            # Remove levels 3 inside level 4
            level3[countries_level3 %in% countries_level4] <- NA
            level3 <- na.omit(level3)

            # Subset map
            m_i <- m[m$ISO_Code %in% country_level |
                       m$Level3_cod %in% level3 |
                       m$Level4_cod %in% level4, ]
            m_i <- terra::aggregate(m_i)
            return(m_i)
          }

          # if(y == "fishbase"){
          #   return(FALSE) #Precisa testar
          # }

        })
        names(spt) <- avx
        return(spt)
      } else {spt <- NULL}

    })
      if(length(d_species)){
      d_species <- unlist(d_species, recursive = FALSE)
    } else{
      names(d_species) <- species
    }
    return(d_species)
  }

  # Final dataset
  if(!return_distribution){
    return(r_species)
  } else {
    return(list("species_info" = r_species,
                "species_range" = d_species))
  }

}

# data_dir <- "C:/Users/wever/Desktop/GitHub/Curso_ENM/Ocorrencias/Dados_especialistas"
# species <- "Araucaria angustifolia"
# return_distribution = FALSE
