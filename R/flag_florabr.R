flag_florabr <- function(data_dir, occ, species = "species",
                         long = "decimalLongitude", lat = "decimalLatitude",
                         origin = NULL, verbose = TRUE, by_endemism = FALSE) {

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
                                     by_endemism = by_endemism,
                                     value = "flag")

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
# by_endemism = FALSE
# origin = NULL
# res <- flag_florabr(data_dir = data_dir, occ = occ, origin = origin,
#                     species = species, long = long,
#                     lat = lat, by_endemism = by_endemism)

