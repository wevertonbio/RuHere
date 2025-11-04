fishbase_here <- function(data_dir, 
                          data_version = "latest",
                          verbose = TRUE) {
  

  if (is.null(data_dir)) {
    stop("data_dir should be specified")
  } else if (!inherits(data_dir, "character")) {
    stop("data_dir should be a character")
  }

  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }
  dir.create(file.path(data_dir, "fishbase"))

  if (!inherits(data_version, "character")) {
    stop("data_version should be a character")
  }

  if (!inherits(verbose, "logical")) {
    stop("verbose should be logical")
  }
  
  if (verbose) message("Downloading complete species list...")
  sp_list <- rfishbase::species(version = data_version)
  names_sp <- rfishbase::species_names()
  comp_list <- sp_list %>%
    dplyr::left_join(names_sp, by = "SpecCode")

  data.table::fwrite(comp_list, 
                     file.path(data_dir, "fishbase/fb_species_list.gz"))
  
  if (verbose) message("Downloading country distribution data (C_Code)...")
  suppressMessages(fb_country <- rfishbase::country(comp_list$Species, 
                                            version = data_version))
  data.table::fwrite(fb_country, 
                     file.path(data_dir, "fishbase/fb_species_country.gz"))
  
  if (verbose) message("Downloading FAO Area distribution data...")  
  suppressMessages(fb_fao <- rfishbase::faoareas(comp_list$Species, 
                                         version = data_version))
  data.table::fwrite(fb_fao, file.path(data_dir, "fishbase/fb_species_fao.gz"))
  
  if (verbose) message("Downloading ecosystem distribution data...")
  suppressMessages(fb_ecosystem <- rfishbase::ecosystem(comp_list$Species, 
                                                version = data_version))
  data.table::fwrite(fb_ecosystem, file.path(data_dir, "fishbase/fb_species_ecosystem.gz"))

  if (verbose) message("Downloading country decoder map...")
  suppressMessages(fb_decoder <- rfishbase::c_code(version = data_version))
  data.table::fwrite(fb_decoder, 
                     file.path(data_dir, "fishbase/fb_countries_decoder.gz"))
}

data_dir = "~/Documents/lab/RuHere/"
fishbase_here(data_dir)