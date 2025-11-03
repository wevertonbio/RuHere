flag_wcvp <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      origin = NULL,
                      buffer = 10, verbose = TRUE){

  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "wcvp/wcvp.gz"))){
    stop("Data from WCVP necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'iucn_here()' function")
  }
  if(!file.exists(file.path(data_dir, "wcvp/wgsrpd.gpkg"))){
    stop("Data from WCVP necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'iucn_here()' function")
  }

  # Import data
  d <- data.table::fread(file.path(data_dir, "wcvp/wcvp.gz"))
  # Import map
  m <- terra::vect(file.path(data_dir, "wcvp/wgsrpd.gpkg"))
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

  res_flag <- pbapply::pblapply(spp_in, function(i){
    # Get data from wcvp
    d_i <- d %>% dplyr::filter(species == i)

    # if origin is not NULL...
    if(!is.null(origin)){
      if(!("native" %in% origin)){
        d_i <- d_i[d_i$introduced == 1 | d_i$extinct == 1 |
                     d_i$location_doubtful == 1,]
      }
      if(!("introduced" %in% origin)){
        d_i <- d_i[d_i$introduced == 0,]
      }
      if(!("extinct" %in% origin)){
        d_i <- d_i[d_i$extinct == 0,]
      }
      if(!("location_doubtful" %in% origin)){
        d_i <- d_i[d_i$location_doubtful == 0,]
      }
    }

    # Subset map
    m_i <- m[m$Level3_cod %in% d_i$LEVEL3_COD]
    m_i <- terra::aggregate(m_i)

    #Add buffer
    if(!is.null(buffer)){
      m_i <- terra::buffer(m_i, width = buffer * 1000)
    }

    # Check if records fall inside
    occ_i <- occ[occ[[species]] == i, ]
    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    occ_i$wcvp_flag <- terra::is.related(pts, m_i, "intersects")

    # # Test
    # plot(m_i)
    # points(pts, col = ifelse(occ_i$wcvp_flag, "green", "red"))

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
# record_id = "record_id"
# origin = NULL
# presence = NULL
# buffer = 10

