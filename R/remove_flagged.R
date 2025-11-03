remove_flagged <- function(occ,
                           species = "species",
                           long = "decimalLongitude",
                           lat = "decimalLatitude",
                           flags = "all",
                           save_flagged = FALSE,
                           output_dir = NULL,
                           output_format = ".gz"){
  if(flags == "all"){
    flags <- c("correct_country", "correct_state", "florabr", "wcvp", "iucn",
               "bien", "cultivated", "inaturalist", "duplicated")
  }

  # Add _flags for some columns
  flags[!grepl("correct", flags)] <- paste0(flags[!grepl("correct", flags)],
                                            "_flag")

  # Subset columns
  flags <- intersect(flags, colnames(occ))

  # Create empty list to save flagged results
  occ_flagged <- list()
   for(i in flags){
    occ_flagged[[i]] <- occ[!occ[[i]],]
   }

  # Filter
  occ_final <- occ %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(flags), ~ . == TRUE))

  if(save_flagged){
    for(i in flags){
      file_i <- file.path(output_dir, paste0(i, output_format))
      data.table::fwrite(x = occ_flagged[[i]],
                         file_i)
    }
  }
  return(occ_final)
}
