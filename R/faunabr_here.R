faunabr_here <- function(data_dir,
                         data_version = "latest",
                         overwrite = TRUE, verbose = TRUE){
  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }

  # Create directory
  dir.create(file.path(data_dir, "faunabr"))

  if(verbose){
    message("Getting data from Taxonomic Catalog of the Brazilian Fauna ...")
  }
  faunabr::get_faunabr(output_dir = file.path(data_dir, "faunabr"),
                       data_version,
                       solve_discrepancies = TRUE,
                       overwrite = overwrite,
                       verbose = verbose)
}

# data_dir <- "../RuHere_test/"
# data_version = "latest"
# overwrite = TRUE
# verbose = TRUE
# faunabr_here(data_dir = "../RuHere_test/")
