remove_flagged <- function(occ,
                           flags = "all",
                           exceptions = NULL,
                           column_exception = NULL,
                           save_flagged = FALSE,
                           output_dir = NULL,
                           overwrite = FALSE,
                           output_format = ".gz"){
  if(flags == "all"){
    flags <- c("correct_country", "correct_state", "florabr", "faunabr",
               "wcvp", "iucn", "bien", "cultivated", "inaturalist",
               "duplicated", "thin",
               # Froom CoordinateCleaner
               ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
               ".gbf", ".inst", ".aohi")
  }

  # Add _flags for some columns
  to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                "inaturalist", "duplicated", "thin")

  flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

  # Subset columns
  flags <- intersect(flags, colnames(occ))

  # Dataframe to change names of flags
  flag_names <- data.frame(flag= c("correct_country",
                                   "correct_state",
                                   "cultivated_flag",
                                   "fossil_flag",
                                   "inaturalist_flag",
                                   "faunabr_flag",
                                   "florabr_flag",
                                   "wcvp_flag",
                                   "iucn_flag",
                                   "duplicated_flag",
                                   "thin_flag",
                                   "No flagged",
                                   ".val",
                                   ".equ",
                                   ".zer",
                                   ".cap",
                                   ".cen",
                                   ".sea",
                                   ".urb",
                                   ".otl",
                                   ".gbf",
                                   ".inst",
                                   ".aohi"),
                           new_name = c("Wrong country",
                                        "Wrong state",
                                        "Cultivated",
                                        "Fossil",
                                        "Inaturalist",
                                        "Outside faunabr",
                                        "Outside florabr",
                                        "Outside WCVP",
                                        "Outside IUCN",
                                        "Duplicated",
                                        "Thinned out",
                                        "No flagged",
                                        "Invalid lat/long",
                                        "Equal lat/long",
                                        "Zero lat/long",
                                        "Capital centroid",
                                        "Country/Province centroid",
                                        "Open sea",
                                        "Urban area",
                                        "Outlier",
                                        "GBIF headquarters",
                                        "Biodiversity Institution",
                                        "Artificial Hotspot Occurrence"),
                           stringsAsFactors = FALSE)

  flag_names <- stats::setNames(flag_names$new_name, flag_names$flag)

  # Exceptions...
  if(!is.null(exceptions)){
    occ_keep <- occ[occ[[column_exception]] %in% exceptions,]
    occ <- occ[!(occ[[column_exception]] %in% exceptions),]
  }

  # Create empty list to save flagged results
  occ_flagged <- list()
  # Identify flags
   for(i in flags){
    occ_flagged[[i]] <- occ[!occ[[i]],]
   }

  # Update flag names
  match_flags <- flag_names[match(names(occ_flagged), names(flag_names))]
  names(occ_flagged) <- match_flags

  # Remove flags with 0 records
  occ_flagged <- occ_flagged[sapply(occ_flagged, function(i) nrow(i) > 0)]


  # Filter
  occ_final <- occ %>%
    dplyr::filter(dplyr::if_all(dplyr::all_of(flags), ~ . == TRUE))

  if(!is.null(exceptions)){
    occ_final <- dplyr::bind_rows(occ_final, occ_keep)
  }

  if(save_flagged){
    # Update flags
    flags <- names(occ_flagged)

    # Build path to sabe
    p <- lapply(flags, function(i){
      file_i <- file.path(output_dir, paste0(i, output_format))
    })

    #Check if files exists
    if(!overwrite){
      f_exists <- sapply(p, file.exists)
      if(sum(f_exists) > 0){
        stop("Some flagged occurrences already exists in '", pasta_removidos, "'.\n",
             "Delete the file, change the folder or set 'overwrite = TRUE'")
      }
    }
  #Save
    for(y in 1:length(flags)){
      data.table::fwrite(x = occ_flagged[[flags[y]]],
                         p[[y]])
    }
  }
  return(occ_final)
}
