flag_faunabr <- function(data_dir, occ, species = "species",
                         long = "decimalLongitude", lat = "decimalLatitude",
                         verbose = TRUE, by_state = TRUE, buffer_state = 20,
                         by_country = TRUE, buffer_country = 20, 
                         keep_columns = TRUE, spat_state = NULL,
                         spat_country = NULL) {

  if (!is.null(data_dir) && !inherits(data_dir, "character")) {
      stop("data_dir must be a character")
  }

  if (!is.null(verbose) && !inherits(verbose, "logical")) {
      stop("verbose must be logical")
  }

  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "faunabr/"))){
    stop("Data from faunabr necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'florabr_here()' function")
  }

  # Import data
  d <- faunabr::load_faunabr(file.path(data_dir, "faunabr/"), type = "complete")
  
  # Get species in data
  spp_in <- intersect(unique(occ[["species"]]),
                      unique(d$species))
  spp_out <- setdiff(unique(occ[["species"]]),
                     unique(d$species))

  #Warning if some species are not available
  if (length(spp_out) > 0) {
    warning("Some species present in occ will not be checked due to absence of information in faunabr")
  }

  if (length(spp_in) == 0) {
    stop("None of the species in occ has information in the faunabr data available")
  }

  if (verbose) {
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique(occ[["species"]])), " species")
  }

  occ_in <- occ[occ[[species]] %in% spp_in, ]

  res_flag <- pbapply::pblapply(spp_in, function(i) {
    
    d_i <- d %>% dplyr::filter(species == i)

    if(!is.null(origin)) {

      if(!origin %in% c("native", "cryptogenic", "exotic")) {
        stop("origin should be 'native', 'cryptogenic', or 'exotic'.")
      }

      d_i$origin <- tolower(d_i$origin)

      if ("native" %in% origin) {
        d_i <- d_i[d_i$origin == "native", ]
      }
      if ("cryptogenic" %in% origin) {
        d_i <- d_i[d_i$origin == "cryptogenic", ]
      }
      if ("exotic" %in% origin) {
        d_i <- d_i[d_i$origin == "EXOTICA", ]
      }
    }

    occ_i <- faunabr::filter_faunabr(data = d_i, occ = occ_in,
                                     species = species,
                                     long = long,
                                     lat = lat,
                                     value = "flag",
                                     by_state = by_state, 
                                     buffer_state = buffer_state,
                                     by_country = by_country, 
                                     buffer_country = buffer_country,
                                     keep_columns = keep_columns,
                                     spat_state = spat_state,
                                     spat_country = spat_country)

    colnames(occ_i)[colnames(occ_i) == "filters_ok"] <- "faunabr_flag"

    return(occ_i)

  })

  res_flag <- data.table::data.table(data.table::rbindlist(res_flag)) %>%
    as.data.frame()

  if (length(spp_out) > 0) {
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$faunabr_flag <- NA
    res_flag <- dplyr::bind_rows(res_flag, occ_out)
  }

  return(res_flag)

}

# occ <- RuHere::occurrences
# occ$species <- "Panthera onca"
# data_dir = "../RuHere_test/"
# species = "species"
# long = "decimalLongitude"
# lat = "decimalLatitude"
# by_endemism = FALSE
# origin = NULL
# res <- flag_faunabr(data_dir = data_dir, occ = occ, 
#                     species = species, long = long,
#                     lat = lat)

