iucn_here <- function(species,
                      synonyms = NULL,
                      data_dir,
                      iucn_credential = NULL,
                      overwrite = TRUE,
                      verbose = TRUE,
                      return_data = TRUE){
  # Check directory
  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }

  # Create directory
  odir <- file.path(data_dir, "iucn")
  dir.create(odir, showWarnings = FALSE)

  # Check iucn api
  if (is.null(iucn_credential)) {
    key <- Sys.getenv("IUCN_REDLIST_KEY")
  } else {
    key <- iucn_credential
  }

  if(key == ""){
    stop("You must get and save an IUCN API key. Check the function 'set_iucn_credentials()")
  }


  # Get species information
  spinfo <- pbapply::pblapply(species, function(i){
    if(!is.null(synonyms)){
      s_i <- synonyms[synonyms[[1]] == i, 2]
      if(length(s_i) > 0){
        spp <- unique(c(i, s_i))
      } else {spp <- i}
    } else {spp <- i}

    # Split binomial name in genus and species
    ss <- strsplit(spp, " ")
    names(ss) <- spp

    r <- lapply(ss, function(x){
      r <- try(rredlist::rl_species(genus = x[1],
                               species = x[2],
                               key = key)$assessments, silent = TRUE)

      if(inherits(r, "data.frame")){
        assessment <- rredlist::rl_assessment(r$assessment_id[r$latest],
                                              key = key)
        if("locations" %in% names(assessment)){
          l <- assessment$locations
          d <- l$description$en
          l <- l %>% dplyr::select(code, origin, presence) %>%
            mutate(species = i,
                   region = d, .before = 1)
        } else { #If there is no info on location
          l <- data.frame(species = i, region = NA, code = NA, origin = NA,
                          presence = NA)
        }
      } else { #If it's not a dataframe
        l <- data.frame(species = i, region = NA, code = NA, origin = NA,
                        presence = NA)
      }
      return(l)
    })
    r <- na.omit(data.table::rbindlist(r))
    return(r)
  })

  spinfo <- distinct(data.table::rbindlist(spinfo))

  # Save results
  data.table::fwrite(x = spinfo,
                     file = file.path(odir, "iucn_distribution.gz"))

  # Get map
  utils::download.file(url = "https://zenodo.org/records/17455838/files/wgsrpd.gpkg?download=1",
                       destfile = file.path(odir, "wgsrpd.gpkg"),
                       method = "auto",
                       mode = "wb",
                       cacheOK = TRUE)

  # Data saved in...
  if(verbose){
    message("Data sucessfully saved in '", odir, "'")
  }

  # If return data...
  if(return_data){
    return(spinfo)
  } else {
    return(invisible(NULL))
  }

}

# species <- c("Araucaria angustifolia", "Panthera onca")
# data_dir <- "../RuHere_test/"
# overwrite = TRUE
# verbose = TRUE

# iucn <- iucn_here(species = c("Araucaria angustifolia", "Panthera onca",
#                               "Cyanocorax coeruleus"),
#                   data_dir = "../RuHere_test/")
