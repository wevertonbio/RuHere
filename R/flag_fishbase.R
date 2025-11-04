flag_fishbase <- function(data_dir, occ, species = "species",
                          long = "decimalLongitude", lat = "decimalLatitude",
                          buffer = 20,
                          verbose = TRUE) {

  if (is.null(data_dir)) {
    stop("data_dir should be specified")
  } else if (!inherits(data_dir, "character")) {
    stop("data_dir should be a character")
  }

  if (is.null(occ)) {
    stop("occ should be specified")
  } else if (!inherits(occ, "data.frame")) {
    stop("occ should be a data.frame")
  }

  if (!inherits(species, "character")) {
    stop("species should be a character")
  }

  if (!inherits(long, "character")) {
    stop("long should be a character")
  }

  if (!inherits(lat, "character")) {
    stop("lat should be a character")
  }
  
  if (!inherits(buffer, "numeric")) {
      stop("buffer must be numeric")
  }

  if (!inherits(verbose, "logical")) {
      stop("verbose must be logical")
  }

  if (!dir.exists(file.path(data_dir, "fishbase/"))) {
    stop("Directory 'fishbase/' not found in ", data_dir,
         ".\\nCheck the folder or run the 'fishbase_here()' function")
  }

  # 1b. Verifica se o arquivo de distribuição por país existe
  if (!file.exists(file.path(data_dir, "fishbase/fb_species_country.gz"))) {
    stop("Data 'fb_species_country.gz' necessary to check records is not available in ", data_dir,
         ".\\nCheck the folder or run the 'fishbase_here()' function")
  }

  # 1c. Verifica se o decoder de países existe
  if (!file.exists(file.path(data_dir, "fishbase/fb_countries_decoder.gz"))) {
    stop("Data 'fb_countries_decoder.gz' (decoder) necessary to check records is not available in ", data_dir,
         ".\\nCheck the folder or run the 'fishbase_here()' function")
  }

  all_species <- NULL
  
  d_country <- data.table::fread(file.path(data_dir, "fishbase/fb_species_country.gz"))
  d_decoder <- data.table::fread(file.path(data_dir, "fishbase/fb_countries_decoder.gz"))
  all_species <- c(all_species, d_country$Species)
    
  m_country <- terra::vect(system.file("extdata/world.gpkg", package = "RuHere"))
  decoder_map <- d_decoder[, c("C_Code", "country")]
  decoder_map$name <- tolower(decoder_map$country)
  decoder_map <- unique(decoder_map[, c("C_Code", "name")])

  d_species <- unique(all_species)
  spp_in <- intersect(unique(occ[[species]]), d_species)
  spp_out <- setdiff(unique(occ[[species]]), d_species)

  if (length(spp_out) > 0) {
    warning("Some species present in occ will not be checked due to absence of information in FishBase")
  }

  if (length(spp_in) == 0) {
    stop("None of the species in occ has information in the FishBase data available")
  }

  if (verbose) {
    message("Checking the distribution of ", length(spp_in), " of ",
            length(unique(occ[[species]])), " species")
  }

  res_flag <- pbapply::pblapply(spp_in, function(i) {
    
    occ_i <- occ[occ[[species]] == i, ]

    m_i_country <- NULL
    occ_i$fishbase_flag <- NA 
    d_i_country <- d_country[d_country$Species == i, ]
      
    if (nrow(d_i_country) > 0) {
      sp_c_codes <- unique(d_i_country$C_Code)
      sp_names_to_check <- decoder_map$name[decoder_map$C_Code %in% sp_c_codes]
      m_country_sub <- m_country[m_country$name %in% sp_names_to_check, ]
      
      if (nrow(m_country_sub) > 0) {
        m_i_country <- terra::aggregate(m_country_sub)
        m_i_country <- terra::buffer(m_i_country, width = buffer * 1000)
      }
    }

    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    if (!is.null(m_i_country)) {
      occ_i$fishbase_flag <- terra::is.related(pts, m_i_country, "intersects")
    }
    
    return(occ_i)

  })

  res_flag <- data.table::rbindlist(res_flag, fill = TRUE) %>% as.data.frame()

  if (length(spp_out) > 0) {
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$fishbase_flag <- NA
    res_flag <- dplyr::bind_rows(res_flag, occ_out)
  }
  
  return(res_flag)
}

# occ <- RuHere::occurrences
# occ$species <- "Thunnus thynnus"
# data_dir = "../RuHere_test/"
# species = "species"
# long = "decimalLongitude"
# lat = "decimalLatitude"
# res <- flag_fishbase(data_dir = data_dir, occ = occ,
#                      species = species, long = long,
#                      lat = lat)

