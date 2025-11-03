redlisted_here <- function(data_dir,
                           overwrite = TRUE,
                           verbose = TRUE,
                           timeout = 600){

  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }

  # Create directory
  odir <- file.path(data_dir, "redlist")
  dir.create(odir, showWarnings = FALSE)

  if(verbose){
    message("Task 1 of 2: Downloading map from the 'Native range estimates for red-listed vascular plants'...\n")
  }


  # Get map
  default_timeout <- options()$timeout
  options(timeout = timeout)
  utils::download.file(url = "https://datadryad.org/api/v2/files/1330968/download",
                       destfile = file.path(odir, "redlist.zip"),
                       method = "auto", mode = "wb",
                       cacheOK = TRUE)
  # Reset timeout
  options(timeout = default_timeout)

  # Unzip file
  utils::unzip(zipfile = file.path(odir, "redlist.zip"),
               files = c("range_data_default.nc", "metadata_default.csv"),
               exdir = odir)


  # Get species ids
  spp_ids <- data.table::fread(file.path(odir, "metadata_default.csv"),
                               select = c("speciesID", "scientificname"))

  a <- terra::sds("../RuHere_test/redlist/range_data_default.nc")
  a2 <- a[["Native region"]]
  names(a2)
  nlyr(a2)
  # a3 <- a2[["Native region_SpeciesID=901"]] %>% trim()
  # a4 <- as.polygons(a3)

  #Rename a2
  names(a2) <- sub("Native region_SpeciesID=", "", names(a2))



  #Get and save polygon of native region of all species
  dir.create("../RuHere_test/redlist/native_ranges")
  spp_native <- pblapply(names(a2), function(x) {
    spp_x <- spp_ids %>% filter(speciesID == x) %>% pull(scientificname)
    rx <- terra::trim(a2[[x]])
    # px <- as.polygons(rx)
    names(rx) <- spp_x
    writeRaster(rx, paste0("../RuHere_test/redlist/native_ranges/",
                           spp_x, ".tif"))
    })






}

# library(terra)
# library(dplyr)
# data_dir <- "../RuHere_test/"
# overwrite = TRUE
# verbose = TRUE
# redlisted_here(data_dir = "../RuHere_test/")
