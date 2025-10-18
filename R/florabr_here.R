florabr_here <- function(data_dir,
                         data_version = "latest",
                         overwrite = TRUE, verbose = TRUE){
  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }
  # Create directory
  dir.create(file.path(data_dir, "florabr"))

  if(verbose){
    message("Getting data from Flora e Funga do Brasil...")
  }
  florabr::get_florabr(output_dir = file.path(data_dir, "florabr"),
                       data_version,
                       solve_discrepancy = TRUE, overwrite = overwrite,
                       verbose = verbose, remove_files = TRUE)

}

# data_dir <- "../RuHere_test/"
# data_version = "latest"
# overwrite = TRUE
# verbose = TRUE
#florabr_here(data_dir = "../RuHere_test/")
