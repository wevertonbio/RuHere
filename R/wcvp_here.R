wcvp_here <- function(data_dir,
                      overwrite = TRUE,
                      verbose = TRUE,
                      remove_files = TRUE){

  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }

  # Create directory
  odir <- file.path(data_dir, "wcvp")
  dir.create(odir, showWarnings = FALSE)


  if(verbose){
    message("Task 1 of 3: Downloading data from the World Checklist of Vascular Plants (WCVP) repository...\n")
  }
  # Downlod file
  utils::download.file(url = "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip",
                       destfile = file.path(odir, "wcvp.zip"),
                       method = "auto",
                       cacheOK = TRUE)


  if(verbose){
    message("Task 2 of 3: Merging data...\n")
  }

  # Unzip file
  utils::unzip(zipfile = file.path(odir, "wcvp.zip"),
               exdir = file.path(odir))

  wcp_names <- data.table::fread(file.path(odir, "wcvp_names.csv"),
                                 select = c("plant_name_id", "taxon_name"))

  colnames(wcp_names)[2] <- "species"
  wcp_dist <- data.table::fread(file.path(odir, "wcvp_distribution.csv"),
                                select = c("plant_name_id", "area_code_l3",
                                           "introduced", "extinct",
                                           "location_doubtful")) %>% distinct()
  colnames(wcp_dist)[2] <- "LEVEL3_COD"
  wcp <- left_join(wcp_names, wcp_dist, by = "plant_name_id") %>% na.omit() %>%
    dplyr::select(-plant_name_id)

  # Save results
  data.table::fwrite(wcp,
                     file.path(odir, "wcvp.gz"))

  if(verbose){
    message("Task 3 of 3: Downloading map from the World Geographical Scheme for Recording Plant Distributions (WGSRPD)...\n")
  }

  # Get map
  utils::download.file(url = "https://github.com/wevertonbio/spatial_files/raw/refs/heads/main/Data/wgsrpd.gpkg",
                       destfile = file.path(odir, "wgsrpd.gpkg"),
                       method = "auto",
                       mode = "wb",
                       cacheOK = TRUE)


  # Remove files
  if(remove_files){
    unlink(file.path(odir, "wcvp_names.csv"), recursive = TRUE, force = TRUE)
    unlink(file.path(odir, "wcvp_distribution.csv"), recursive = TRUE, force = TRUE)
    unlink(file.path(odir, "wcvp.zip"), recursive = TRUE, force = TRUE)
    unlink(file.path(odir, "README_WCVP.xlsx"), recursive = TRUE, force = TRUE)
  }


  if(verbose){
    message("Finished!\n")
  }

  message("Please don't forget to cite:\n
          Govaerts, R., Nic Lughadha, E. et al. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. Sci Data, 8, 215 (2021). https://doi.org/10.1038/s41597-021-00997-6")
}

# data_dir <- "../RuHere_test/"
# overwrite = TRUE
# verbose = TRUE
# remove_files = TRUE
# wcvp_here(data_dir = "../RuHere_test/")
