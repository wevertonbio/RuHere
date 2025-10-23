bien_here <- function(data_dir,
                      species,
                      overwrite = TRUE,
                      verbose = TRUE){

  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }

  # Create directory
  odir <- file.path(data_dir, "bien")
  dir.create(odir, showWarnings = FALSE)

  #### PARALELIZAR... ####

  # Save species
  res <- pbapply::pblapply(species, function(i){
    sp_i <- sub(" ", "_", i)
    v_i <- BIEN::BIEN_ranges_load_species(species = sp_i, fetch.query = FALSE)
    if(nrow(v_i) > 0){
      v_i <- terra::vect(v_i)
      terra::writeVector(v_i, file.path(odir,
                                        paste0(i, ".gpkg")),
                         overwrite = overwrite)
      return(data.frame(species = i, range_available = TRUE))
    } else {return(data.frame(species = i, range_available = FALSE))}
  })
  return(data.table::rbindlist(res))
}

# data_dir <- "../RuHere_test/"
# species = c("Araucaria araucana", "Araucaria angustifolia")
# overwrite = TRUE
# verbose = TRUE
# bien_here(data_dir = "../RuHere_test/",
#           species = species)


# library(BIEN)
# library(terra)
#
# l <- BIEN_ranges_list()
#
# a <- BIEN::BIEN_ranges_species(species = "Myrceugenia_reitzii",
#                                directory = tempdir())
#
# b <- vect(file.path(tempdir(), "Myrceugenia_reitzii.shp"))
# plot(b)
# b
# mapview::mapview(b)
#
# a <- BIEN::BIEN_ranges_load_species(species = "Araucaria_araucana")
# class(a)
# a <- vect(a)
# plot(a)
#
# z <- vect("C:/Users/wever/Downloads/Myrceugenia_campestris_range/TP05__Myrceugenia_campestris__mean__noBias_1e.06_0_1e.06_0_all_all_none_all_maxnet_none_equalWeights.shp")
# plot(z)
