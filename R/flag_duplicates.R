flag_duplicates <- function(occ,
                            species = "species",
                            long = "decimalLongitude",
                            lat = "decimalLatitude",
                            additional_groups = NULL,
                            continuous_variable = NULL,
                            decreasing = TRUE,
                            categorical_variable = NULL,
                            priority_categories = NULL,
                            by_cell = FALSE,
                            raster_variable = NULL){


  # Extract ID of rasters if by_cell
  if(by_cell){

    if(is.null(raster_variable)){
      stop("If 'by_cell = TRUE', 'raster_variable' must be provided")
    }

    if(!inherits(raster_variable, "SpatRaster")){
      stop("If 'by_cell = TRUE', raster_variable must be a 'SpatRaster', not a ",
           class(raster_variable))
    }

    occ$cell_id <- terra::extract(raster_variable[[1]],
                              occ[, c(long, lat)], cells = TRUE)[["cell"]]

    to_group <- c(species, "cell_id")
  } else {
    to_group <- c(species, long, lat)
  }

  if(!is.null(additional_groups)){
    to_group <- c(to_group, additional_groups)
  }

  if(is.null(continuous_variable) & is.null(categorical_variable)){
    occ <- occ %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(to_group))) %>%
      dplyr::mutate(duplicated_flag = (dplyr::row_number() == 1)) %>%
      dplyr::ungroup()
  }

  if(!is.null(continuous_variable) & is.null(categorical_variable)){
    occ <- occ %>%
      dplyr::arrange(dplyr::desc(!!dplyr::sym(continuous_variable))) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(to_group))) %>%
      dplyr::mutate(duplicated_flag = (dplyr::row_number() == 1)) %>%
      dplyr::ungroup()
  }

  if(!is.null(categorical_variable) & is.null(continuous_variable)){
    occ <- occ %>%
      dplyr::arrange(match(!!dplyr::sym(categorical_variable),
                           priority_categories)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(to_group))) %>%
      dplyr::mutate(duplicated_flag = (dplyr::row_number() == 1)) %>%
      dplyr::ungroup()
  }

  if(!is.null(categorical_variable) & !is.null(continuous_variable)){
    occ <- occ %>%
      dplyr::arrange(dplyr::desc(!!dplyr::sym(continuous_variable)),
                     match(!!dplyr::sym(categorical_variable),
                           priority_categories)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(to_group))) %>%
      dplyr::mutate(duplicated_flag = (dplyr::row_number() == 1)) %>%
      dplyr::ungroup()
  }

  return(occ)
}


# occ_dup <- remove_duplicates(occ = occurrences, species = "scientificName",
#                              long = "decimalLongitude",
#                              lat <- "decimalLatitude",
#                              continuous_variable = "year",
#                              categorical_variable = "data_source",
#                              priority_categories = c("gbif", "specieslink",
#                                                      "bien", "fake_data"),
#                              decreasing = TRUE)
# sum(!occ_dup$duplicated_flag)
# occ_dup <- remove_duplicates(occ = occurrences, species = "scientificName",
#                              long = "decimalLongitude",
#                              lat <- "decimalLatitude",
#                              continuous_variable = "year",
#                              decreasing = TRUE, by_cell = TRUE,
#                              raster_variable = raster_variable)
# sum(!occ_dup$duplicated_flag)
#
# occ <- occurrences
# species = "scientificName"
# long = "decimalLongitude"
# lat <- "decimalLatitude"
# continuous_variable = "year"
# decreasing = TRUE
# categorical_variable = "data_source"
# priority_categories = c("gbif", "specieslink", "bien", "fake_data")
# by_pixel = TRUE
# raster_variable <- rast("C:/Users/wever/Downloads/wc2.1_10m_bio/wc2.1_10m_bio_1.tif")
# plot(raster_variable)
