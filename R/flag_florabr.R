flag_florabr <- function(data_dir, occ, species = "species",
                         long = "decimalLongitude", lat = "decimalLatitude",
                         verbose = TRUE, origin = NULL, 
                         by_state = TRUE, buffer_state = 20,
                         by_biome = TRUE, buffer_biome = 20, by_endemism = TRUE,
                         buffer_brazil = 20, state_vect = NULL,
                         state_column = NULL, biome_vect = NULL,
                         biome_column = NULL, br_vect = NULL,
                         keep_columns = TRUE) {

  if (!is.null(data_dir) && !inherits(data_dir, "character")) {
      stop("data_dir must be a character")
  }

  if (!is.null(verbose) && !inherits(verbose, "logical")) {
      stop("verbose must be logical")
  }
  
  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "florabr/"))){
    stop("Data from florabr necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'florabr_here()' function")
  }

  # Import data
  d <- florabr::load_florabr(file.path(data_dir, "florabr/"), type = "complete")
  
  # Get species in data
  spp_in <- intersect(unique(occ[["species"]]),
                      unique(d$species))
  spp_out <- setdiff(unique(occ[["species"]]),
                     unique(d$species))

  #Warning if some species are not available
  if (length(spp_out) > 0) {
    warning("Some species present in occ will not be checked due to absence of information in florabr")
  }

  if (length(spp_in) == 0) {
    stop("None of the species in occ has information in the florabr data available")
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique(occ[["species"]])), " species")
  }

  occ_in <- occ[occ[[species]] %in% spp_in, ]

  res_flag <- pbapply::pblapply(spp_in, function(i) {
    
    d_i <- d %>% dplyr::filter(species == i)

    if(!is.null(origin)) {

      if(!origin %in% c("native", "cultivated", "naturalized", "unknown", 
                        "not_found_in_brazil")) {
        stop("origin should be 'native', 'cultivated', 'naturalized', 'unknown', or 'not_found_in_brazil'.")
      }

      d_i$origin <- tolower(d_i$origin)

      if ("native" %in% origin) {
        d_i <- d_i[d_i$origin == "native", ]
      }
      if ("cultivated" %in% origin) {
        d_i <- d_i[d_i$origin == "cultivated", ]
      }
      if ("naturalized" %in% origin) {
        d_i <- d_i[d_i$origin == "naturalized", ]
      }
      if ("unknown" %in% origin) {
        d_i <- d_i[d_i$origin == "unknown", ]
      }
      if ("not_found_in_brazil" %in% origin) {
        d_i <- d_i[d_i$origin == "not_found_in_brazil", ]
      }
    }

    occ_i <- florabr::filter_florabr(data = d_i, occ = occ_in,
                                     species = species,
                                     long = long,
                                     lat = lat,
                                     value = "flag",
                                     by_state = by_state, 
                                     buffer_state = buffer_state,
                                     by_biome = by_biome, 
                                     buffer_biome = buffer_biome, 
                                     by_endemism = by_endemism,
                                     buffer_brazil = buffer_brazil, 
                                     state_vect = state_vect,
                                     state_column = state_column, 
                                     biome_vect = biome_vect,
                                     biome_column = biome_column, 
                                     br_vect = br_vect,
                                     keep_columns = keep_columns)

    colnames(occ_i)[colnames(occ_i) == "filters_ok"] <- "florabr_flag"

    return(occ_i)

  })

  res_flag <- data.table::data.table(data.table::rbindlist(res_flag)) %>%
    as.data.frame()

  if (length(spp_out) > 0) {
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$florabr_flag <- NA
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
# res <- flag_florabr(data_dir = data_dir, occ = occ, origin = origin,
#                     species = species, long = long,
#                     lat = lat)

