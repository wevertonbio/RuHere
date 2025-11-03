flag_iucn <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      origin = NULL, presence = NULL,
                      buffer = 10, verbose = TRUE){

  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "iucn/iucn_distribution.gz"))){
    stop("Data from IUCN necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'iucn_here()' function")
  }
  if(!file.exists(file.path(data_dir, "iucn/wgsrpd.gpkg"))){
    stop("Data from IUCN necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'iucn_here()' function")
  }

  # Import data
  d <- data.table::fread(file.path(data_dir, "iucn/iucn_distribution.gz"))
  # Import map
  m <- terra::vect(file.path(data_dir, "iucn/wgsrpd.gpkg"))
  # Get dataframe to filter levels
  dm <- terra::as.data.frame(m)

  # Get species in data
  spp_in <- intersect(unique(occ[["species"]]),
                      unique(d$species))
  spp_out <- setdiff(unique(occ[["species"]]),
                       unique(d$species))

  #Warning if some species are not available
  if(length(spp_out) > 0){
    warning("Some species present in occ will not be checked due to absence of information in IUCN")
  }

  if(length(spp_in) == 0){
    stop("None of the species in occ has information in the IUCN data available")
  }

  if(verbose){
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique(occ[["species"]])), " species")
  }

  # Flag
  res_flag <- pbapply::pblapply(spp_in, function(i){
    # Get data from iucn
    d_i <- d %>% dplyr::filter(species == i)
    # Filter by origin and presence
    if(!is.null(origin)){
      d_i$origin <- tolower(d_i$origin)
      d_i <- d_i %>% dplyr::filter(origin %in% origin)
    }

    if(!is.null(presence)){
      d_i$presence <- tolower(d_i$presence)
      d_i <- d_i %>% dplyr::filter(presence %in% presence)
    }

    # Get regions of occurrence (by level)
    country_level <- d_i$code[nchar(d_i$code) == 2]
    level3 <- d_i$code[nchar(d_i$code) == 3]
    countries_level3 <- dm$ISO_Code[dm$Level3_cod %in% level3]
    level4 <- d_i$code[nchar(d_i$code) > 3]
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

    #Add buffer
    if(!is.null(buffer)){
      m_i <- terra::buffer(m_i, width = buffer * 1000)
    }

    # Check if records fall inside
    occ_i <- occ[occ[[species]] == i, ]
    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    occ_i$iucn_flag <- terra::is.related(pts, m_i, "intersects")
    # Flags
    return(occ_i)
    })
  res_flag <- data.table::rbindlist(res_flag) %>% as.data.frame()

  # Append occurrences of spp_out, if exists
  if(length(spp_out) > 0){
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$iucn_flag <- NA
    res_flag <- dplyr::bind_rows(res_flag, occ_out)
    }
  return(res_flag)
}

# occ <- RuHere::occurrences
# occ$species <- "Paubrasilia echinata"
# data_dir = "../RuHere_test/"
# species = "species"
# long = "decimalLongitude"
# lat = "decimalLatitude"
# origin = NULL
# presence = NULL
# buffer = 10
