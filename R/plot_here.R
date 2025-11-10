plot_here <- function(occ,
                      flags = "all",
                      show_no_flagged = TRUE,
                      plot_with = "mapview",
                      cex = 6,
                      lwd = 2,
                      col_points = NULL,
                      label = NULL
                      ){

  # Force occ to be a dataframe
  if(inherits(occ, "data.table") | inherits(occ, "spatialvalid"))
    occ <- as.data.frame(occ)

  if(flags == "all"){
    flags <- c("correct_country", "correct_state", "cultivated_flag",
               "fossil_flag", "inaturalist_flag", "florabr_flag",
               "faunabr_flag",  "wcvp_flag", "iucn_flag", "duplicated_flag",
               # From CoordinateCleaner
               ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
               ".gbf", ".inst", ".aohi",
               # From thin_flag
               "thin_flag")
    flags <- intersect(flags, colnames(occ))
  }

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
                                  ".aohi",
                                  "thin_flag"),
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
                                       "Artificial Hotspot Occurrence",
                                       "Thinned out"),
                          stringsAsFactors = FALSE)
  flag_names <- stats::setNames(flag_names$new_name, flag_names$flag)


  # Split points by flag
  occ_list <- lapply(flags, function(i){
    occ[!occ[[i]],]
  })
  names(occ_list) <- flags

  # Remove flags with 0 records
  occ_list <- occ_list[sapply(occ_list, function(x) nrow(x) > 0)]

  # Add non-flagged occurrences?
  if(show_no_flagged){
    no_flagged <- rowSums(occ[, flags, drop = FALSE], na.rm = TRUE)
    no_flagged <- no_flagged == length(flags)
    no_flagged <- occ[no_flagged,]
    occ_list[["No flagged"]] <- no_flagged
  }

  # Spatialize
  occ_list <- lapply(occ_list, spatialize)

  # Update flags
  flags <- names(occ_list)

  # If colors is NULL, use default color
  if(is.null(col_points)){
    col_points <- c("correct_country" = "#E69F00",
                    "correct_state" = "#56B4E9",
                    "cultivated_flag" = "#16FF32",
                    "fossil_flag" = "black",
                    "inaturalist_flag" = "#F0E442",
                    "florabr_flag" = "#782AB6",
                    "faunabr_flag" = "#782AB6",
                    "wcvp_flag" = "#D55E00",
                    "iucn_flag" = "#CC79A7",
                    "duplicated_flag" = "#FFFF80",
                    "No flagged" = "#009E73",
                    ".val" = "gray22",
                    ".equ" = "#F7E1A0",
                    ".zer" = "#85660d",
                    ".cap" = "red",
                    ".cen" = "orange3",
                    ".sea" = "darkblue",
                    ".urb" = "gray",
                    ".otl" = "pink",
                    ".gbf" = "#B00068",
                    ".inst" = "firebrick",
                    ".aohi" = "#2ED9FF",
                    "thin_flag" = "red")
  }


  if(plot_with == "mapview"){
    # Plot first map
    first_map <- names(occ_list)[1]
    mapa <- mapview::mapview(occ_list[[first_map]],
                             col.regions = col_points[[first_map]],
                             cex = cex, lwd = lwd,
                             layer.name = flag_names[[first_map]],
                             label = label)
    # Add other maps
    if(length(flags) > 1){
      for(i in 2:length(flags)){
        flag_i <- flags[i]
        mapa <- mapa + mapview::mapview(occ_list[[flag_i]],
                               col.regions = col_points[[flag_i]],
                               cex = cex, lwd = lwd,
                               layer.name = flag_names[[flag_i]],
                               label = label)
      }
    }

  } #End of plot with mapview
  return(mapa)

}


# occ = occ_iucn
# flags = "all"
# plot_with = "mapview"
# cex = 6
# lwd = 2
# burst = TRUE
# col_points = NULL
# label = "record_id"
