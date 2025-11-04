flag_CoordinateCleaner <- function(occ, species = "species",
                                   long = "decimalLongitude", 
                                   lat = "decimalLatitude",
                                   verbose = TRUE, 
                                   countries = NULL,
                                   tests = c("capitals", "centroids", "equal", 
                                             "gbif", "institutions", 
                                             "outliers", "seas", "zeros"),
                                   capitals_rad = 10000,
                                   centroids_rad = 1000,
                                   centroids_detail = "both",
                                   inst_rad = 100,
                                   outliers_method = "quantile",
                                   outliers_mtp = 5,
                                   outliers_td = 1000,
                                   outliers_size = 7,
                                   range_rad = 0,
                                   zeros_rad = 0.5,
                                   capitals_ref = NULL,
                                   centroids_ref = NULL,
                                   country_ref = NULL,
                                   country_refcol = "iso_a3",
                                   country_buffer = NULL,
                                   inst_ref = NULL,
                                   range_ref = NULL,
                                   seas_ref = NULL,
                                   seas_scale = 50,
                                   seas_buffer = NULL,
                                   urban_ref = NULL,
                                   aohi_rad = NULL,
                                   value = "spatialvalid",
                                   report = FALSE) {

  if (!is.null(verbose) && !inherits(verbose, "logical")) {
      stop("verbose must be logical")
  }

  if (verbose) {
    message("Checking the distribution from ",
            length(unique(occ[["species"]])), " species")
  }

  
  res_flag <- CoordinateCleaner::clean_coordinates(
    x = occ, species = species,
    lon = long, lat = lat, countries = countries, tests = tests,
    capitals_rad = capitals_rad, centroids_rad = centroids_rad,
    centroids_detail = centroids_detail, inst_rad = inst_rad,
    outliers_method = outliers_method, outliers_mtp = outliers_mtp,
    outliers_td = outliers_td, outliers_size = outliers_size,
    range_rad = range_rad, zeros_rad = zeros_rad, capitals_ref = capitals_ref,
    centroids_ref = centroids_ref, country_ref = country_ref,
    country_refcol = country_refcol, country_buffer = country_buffer,
    inst_ref = inst_ref, range_ref = range_ref, seas_ref = seas_ref,
    seas_scale = seas_scale, seas_buffer = seas_buffer, urban_ref = urban_ref,
    aohi_rad = aohi_rad, value = value, report = report
  )

  res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_summary = .summary)

  if (".val" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_val = .val)
  }

  if (".equ" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_equal = .equ)
  }

  if (".zer" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_zeros = .zer)
  }

  if (".cap" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_capitals = .cap)
  }

  if (".cen" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_centroids = .cen)
  }

  if (".sea" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_sea = .sea)
  }
  
  if (".otl" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_outliers = .otl)
  }
  
  if (".gbf" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_gbif = .gbf)
  }
  
  if (".inst" %in% names(res_flag)) {
    res_flag <- res_flag %>%
    dplyr::rename(flag_CoordinateCleaner_institutions = .inst)
  }

  return(res_flag)
}

# occ <- RuHere::occurrences
# occ$species <- "Paubrasilia echinata"
# species = "species"
# long = "decimalLongitude"
# lat = "decimalLatitude"
# tests = c("capitals", "centroids", "equal", "gbif", "institutions", 
#           "outliers", "seas", "zeros")
# res <- flag_CoordinateCleaner(occ = occ, species = species, 
#                               long = long, lat = lat, tests = tests)

