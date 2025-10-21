iucn_here <- function(species,
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

  # Split binomial name in genus and species
  spp <- strsplit(species, " ")
  names(spp) <- species

  # Get species information
  spinfo <- pbapply::pblapply(species, function(sp){
    r <- try(rredlist::rl_species(genus = spp[[sp]][1],
                              species = spp[[sp]][2],
                              key = key)$assessments, silent = TRUE)

    if(inherits(r, "data.frame")){
      assessment <- rredlist::rl_assessment(r$assessment_id[r$latest],
                                          key = key)
      if("locations" %in% names(assessment)){
        l <- assessment$locations
        d <- l$description$en
        l <- l %>% dplyr::select(code, origin, presence) %>%
          mutate(species = sp,
                 region = d, .before = 1)
    } else { #If there is no info on location
      l <- data.frame(species = sp, region = NA, code = NA, origin = NA,
                      presence = NA)
    }
    } else { #If it's not a dataframe
      l <- data.frame(species = sp, region = NA, code = NA, origin = NA,
                      presence = NA)
    }
    return(l)
  })
  spinfo <- data.table::rbindlist(spinfo)

  # Save results
  data.table::fwrite(x = spinfo,
                     file = file.path(odir, "iucn_distribution.gz"))

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

iucn <- iucn_here(species = c("Araucaria angustifolia", "Panthera onca",
                              "Cyanocorax coeruleus"),
                  data_dir = "../RuHere_test/")
