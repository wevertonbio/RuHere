#' Static visualization of occurrence flags with ggplot2
#'
#' @description
#' Produces a static map using **ggplot2**, highlighting flagged records.
#'
#' @export
ggmap_here <- function(occ,
                       long = "decimalLongitude",
                       lat = "decimalLatitude",
                       flags = "all",
                       additional_flags = NULL,
                       names_additional_flags = NULL,
                       col_additional_flags = NULL,
                       show_no_flagged = TRUE,
                       col_points = NULL,
                       size_points = 1,
                       continent = NULL,
                       continent_fill = "gray70",
                       continent_linewidth = 0.3,
                       continent_border = "white",
                       ocean_fill = "aliceblue",
                       extension = NULL,
                       facet = FALSE,
                       ...) {

  if(flags == "all"){
    flags <- c("correct_country", "correct_state", "florabr", "faunabr",
               "wcvp", "iucn", "bien", "cultivated", "inaturalist",
               "duplicated", "thin_env", "thin_geo", "consensus",
               # Froom CoordinateCleaner
               ".val", ".equ", ".zer", ".cap", ".cen", ".sea", ".urb", ".otl",
               ".gbf", ".inst", ".aohi")
  }

  # Add _flags for some columns
  to_paste <- c("florabr", "faunabr", "wcvp", "iucn", "bien", "cultivated",
                "inaturalist", "duplicated", "thin_env", "thin_geo", "consensus")

  flags[flags %in% to_paste] <- paste0(flags[flags %in% to_paste], "_flag")

  # Additional flags
  if(!is.null(additional_flags)){
    flags <- c(flags, additional_flags)
  }

  # Subset columns
  flags <- intersect(flags, colnames(occ))

  # Names of flags
  flag_names <- getExportedValue("RuHere", "flag_names")

  # Names of additional flags
  # Get name
  if(!is.null(additional_flags)){
    if(is.null(names_additional_flags)){
      names_additional_flags <- additional_flags
    }
    names(names_additional_flags) <- additional_flags
    flag_names <- c(flag_names, names_additional_flags)
  }


  # Split points by flag
  occ_list <- lapply(flags, function(i){
    occ[!occ[[i]],]
  })
  #Rename
  names(occ_list) <- flag_names[flags]


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
  occ_list <- lapply(occ_list, spatialize, long = long, lat = lat)

  # Update flags
  flags <- names(occ_list)

  # If colors is NULL, use default color
  if(is.null(col_points)){
    col_points <- getExportedValue("RuHere", "flag_colors")
  }

  # Append color of additional flags
  if(!is.null(additional_flags)){
    col_additional <- col_additional_flags
    names(col_additional) <- names_additional_flags
    col_points <- c(col_points, col_additional)
  }

  # Filter maps with 0 geometries
  occ_list <- occ_list[sapply(occ_list, function(x) length(x) > 0)]

  # Import basemap
  if(is.null(continent)){
    continent <- terra::vect(system.file("extdata/world.shp",
                                         package = "RuHere"))
  }

  # Merge occ_list
  pts <- lapply(names(occ_list), function(i){
    pts_i <- occ_list[[i]]
    pts_i$Flag = i
    pts_i[, "Flag"]
  })
  pts <- vect(pts)

  # Define extension
  if(is.null(extension)){
    extension <- terra::ext(pts)
    extension <- c(extension[1] - 0.5, extension[2] + 0.5,
                   extension[3] - 0.5, extension[4] + 0.5)
  }

  # Set order
  pts$Flag <- factor(x = pts$Flag, levels = unique(pts$Flag))

  # Plot using tidyterra and ggplot
  p <- ggplot2::ggplot() +
    tidyterra::geom_spatvector(data = continent,
                               fill = continent_fill,
                               linewidth = continent_linewidth,
                               col = continent_border) +
    tidyterra::geom_spatvector(data = pts, aes(fill = Flag, col = Flag),
                               size = size_points) +
    ggplot2::scale_color_manual(values = flag_colors) +
    ggplot2::coord_sf(xlim = c(extension[1], extension[2]),
             ylim = c(extension[3], extension[4]),
             expand = F) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = ocean_fill,
                                                   colour = NA))

  if(facet){
  p <- p + ggplot2::facet_wrap(.~Flag) +
    theme(legend.position = "none")
  }

  return(p)
}
