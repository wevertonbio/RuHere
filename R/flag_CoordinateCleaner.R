#' @title flag_CoordinateCleaner
#'
#' @usage flag_CoordinateCleaner(occ, species = "species", long = "decimalLongitude",
#' lat = "decimalLatitude", verbose = TRUE, countries = NULL,
#' tests = c("capitals", "centroids", "equal", "gbif", "institutions", "outliers",
#' "seas", "zeros"), capitals_rad = 10000, centroids_rad = 1000,
#' centroids_detail = "both", inst_rad = 100, outliers_method = "quantile",
#' outliers_mtp = 5, outliers_td = 1000, outliers_size = 7, range_rad = 0,
#' zeros_rad = 0.5, capitals_ref = NULL, centroids_ref = NULL, country_ref = NULL,
#' country_refcol = "iso_a3", country_buffer = NULL, inst_ref = NULL,
#' range_ref = NULL, seas_ref = NULL, seas_scale = 50, seas_buffer = NULL,
#' urban_ref = NULL, aohi_rad = NULL, value = "spatialvalid", report = FALSE)
#'
#' @description
#' Flags (validates) occurrence records by running common spatial filtering tests
#' from the \code{CoordinateCleaner} package. This function wraps
#' \code{CoordinateCleaner::clean_coordinates} and standardizes the output column
#' names for easier interpretation and integration into a workflow. Records are
#' flagged for various issues like being near country capitals, institutional
#' centroids, or being spatial outliers.
#'
#' @param occ (data.frame) a data frame containing the occurrence records
#' to be flagged. Must contain columns for species, longitude, and latitude. **Required.**
#' @param species (character) the name of the column in occ that contains the
#' species scientific names. Default is `species`.
#' @param long (character) the name of the column in occ that contains the
#' longitude values. Default is `decimalLongitude`.
#' @param lat (character) Tte name of the column in occ that contains the
#' latitude values. Default is `decimalLatitude`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE.`
#' @param countries (character) a vector of countries (ISO2 or ISO3 codes)
#' to limit the check to. Only records assigned to one of these countries will be checked.
#' Default is `NULL` (no country restriction).
#' @param tests (character) a vector of tests to run (`"capitals"`, `"centroids"`,
#' `"equal"`, `"gbif"`, `"institutions"`, `"outliers"`, `"seas"`, `"zeros"`,
#' `"countries"`, `"duplicates"`, `"ranges"`, "`urban"`).
#' Default is (`"capitals"`, `"centroids"`, `"equal"`, `"gbif"`, `"institutions"`,
#' `"outliers"`, `"seas"`, `"zeros"`).
#' @param capitals_rad (numeric) radius around capitals (in meters) to flag records.
#' Default is 10000 m.
#' @param centroids_rad (numeric) radius around centroids (in meters) to flag records.
#' Default is 1000 m.
#' @param centroids_detail (character) retail for centroids check (`"country"`,
#' `"provinces"`, or `"both"`). Default is `"both"`.
#' @param inst_rad (numeric) radius around biodiversity institutions (in meters)
#' to flag records. Default is 100 m.
#' @param outliers_method (character) method for the outlier test. Default is `"quantile"`.
#' @param outliers_mtp (numeric) multiplier for the outlier distance test.
#' Default is 5.
#' @param outliers_td (numeric) the minimum distance (in kilometers) for the
#' distance-based outlier test. Default is 1000 km.#'
#' @param outliers_size (numeric) minimum number of records per species to run
#' the outlier test. Default is 7.
#' @param range_rad (numeric) radius (in degrees) to flag records outside the
#' known geographic range. Default is 0 (no range check).
#' @param zeros_rad (numeric) radius (in degrees) to flag records that are
#' near the center of the coordinate system (0/0). Default is 0.5 degrees.
#' @param capitals_ref (data.frame) custom data set for country
#' capital coordinates. default uses CoordinateCleaner's internal data.
#' @param centroids_ref (data.frame) custom data set for country/state
#' centroid coordinates. Default uses CoordinateCleaner's internal data.
#' @param country_ref (SpatVector) custom spatial reference for
#' the geographic validity ("gbif") test. Default uses CoordinateCleaner's
#' internal data.
#' @param country_refcol (character) the name of the column in country_ref
#' used to match country information. Default is "iso_a3".
#' @param country_buffer (numeric) buffer (in degrees) applied to
#' country borders during the geographic validity test. Default is `NULL`.
#' @param inst_ref (data.frame) custom data set for biodiversity
#' institutional coordinates. Default uses CoordinateCleaner's internal data.
#' @param range_ref (data.frame) custom data set for species'
#' geographic range information. Default is `NULL`.
#' @param seas_ref (SpatVector) custom spatial layer for the
#' sea check. Default uses CoordinateCleaner's internal data.
#' @param seas_scale (numeric) scale of the map used for the sea check (e.g., 50, 110).
#' Default is 50 (medium resolution).
#' @param seas_buffer (numeric) buffer (in degrees) applied to the
#' coastline during the sea check. Default is `NULL`.
#' @param urban_ref (SpatVector) custom spatial layer for the
#' urban area check (test "urban", not in default tests). Default is NULL.
#' @param aohi_rad (numeric) Radius (in meters) around "all other human
#' habitation" points to flag records. Default is `NULL`.
#' @param value (character) the output value type returned by clean_coordinates.
#' Default is "spatialvalid", which returns the output data frame with validation columns.
#' @param report (logical) whether a summary is printed to the console.
#' Default is `FALSE`.
#'
#' @return A \code{data.frame} that is the original \code{occ} data frame augmented
#' with new columns named \code{flag_CoordinateCleaner_*}. These logical columns
#' indicate the results of the spatial tests, where \code{FALSE} indicates a
#' flagged (potentially erroneous) record. The summary column is named
#' \code{flag_CoordinateCleaner_summary}.
#'
#' @importFrom CoordinateCleaner clean_coordinates
#' @importFrom dplyr rename
#' @importFrom dplyr rename %>%
#' @importFrom stats setNames
#'
#' @export
#'
#' @examples
#' \dontrun{
#' occ <- RuHere::occurrences
#' occ$species <- "Paubrasilia echinata"
#'
#' tests_to_run <- c(
#'   "capitals", "centroids", "equal", "gbif",
#'   "institutions", "outliers", "seas", "zeros"
#' )
#'
#' res <- flag_CoordinateCleaner(
#'   occ = occ,
#'   species = "species",
#'   long = "decimalLongitude",
#'   lat = "decimalLatitude",
#'   tests = tests_to_run
#' )
#' }
#'
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

  if (missing(occ) || !inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame and cannot be missing.")
  }

  col_args <- list(species = species, long = long, lat = lat)
  for (arg_name in names(col_args)) {
    arg_value <- col_args[[arg_name]]
    if (!inherits(arg_value, "character") || length(arg_value) != 1) {
      stop(paste0("'", arg_name, "' must be a single character string, not ", class(arg_value)))
    }
  }

  core_cols <- c(species, long, lat)
  if (!all(core_cols %in% names(occ))) {
    missing_cols <- setdiff(core_cols, names(occ))
    stop("The following columns specified by 'species', 'long', or 'lat' are missing from 'occ': ", paste(missing_cols, collapse = ", "))
  }

  logicals <- list(verbose = verbose, report = report)
  for (arg_name in names(logicals)) {
    arg_value <- logicals[[arg_name]]
    if (!inherits(arg_value, "logical") || length(arg_value) != 1) {
      stop(paste0("'", arg_name, "' must be a single logical value (TRUE/FALSE), not ", class(arg_value)))
    }
  }

  if (!is.null(countries) && !inherits(countries, "character")) {
    stop("'countries' must be a character vector of country codes or NULL, not ", class(countries))
  }

  if (!is.null(tests)) {
    if (!inherits(tests, "character")) {
      stop("'tests' must be a character vector or NULL, not ", class(tests))
    }
    if (!all(tolower(tests) %in% c("capitals", "centroids", "equal", "gbif",
                                   "institutions", "outliers", "seas", "zeros",
                                   "countries", "duplicates", "ranges", "urban",
                                   "aohi"))) {
      stop("'tests' contains invalid values. Must be one or more of: 'capitals',
           'centroids', 'equal', 'gbif', 'institutions', 'outliers', 'seas',
           'zeros', 'urban', 'countries', 'ranges', 'duplicates', 'aohi'")
    }
  }

  numeric_args <- list(
    capitals_rad = capitals_rad, centroids_rad = centroids_rad,
    inst_rad = inst_rad, outliers_mtp = outliers_mtp,
    outliers_td = outliers_td, outliers_size = outliers_size,
    range_rad = range_rad, zeros_rad = zeros_rad, seas_scale = seas_scale
  )
  for (arg_name in names(numeric_args)) {
    arg_value <- numeric_args[[arg_name]]
    if (!inherits(arg_value, "numeric") || length(arg_value) != 1) {
      stop(paste0("'", arg_name, "' must be a single numeric value, not ", class(arg_value)))
    }
  }

  numeric_or_null_args <- list(
    country_buffer = country_buffer, seas_buffer = seas_buffer, aohi_rad = aohi_rad
  )
  for (arg_name in names(numeric_or_null_args)) {
    arg_value <- numeric_or_null_args[[arg_name]]
    if (!is.null(arg_value) && (!inherits(arg_value, "numeric") || length(arg_value) != 1)) {
      stop(paste0("'", arg_name, "' must be a single numeric value or NULL, not ", class(arg_value)))
    }
  }

  if (!inherits(centroids_detail, "character") || length(centroids_detail) != 1 ||
      !(centroids_detail %in% c("country", "provinces", "both"))) {
    stop("'centroids_detail' must be a single character: 'country', 'provinces', or 'both'.")
  }

  if (!inherits(outliers_method, "character") || length(outliers_method) != 1) {
    stop(paste0("'outliers_method' must be a single character, not ", class(outliers_method)))
  }

  single_char_args <- list(
    country_refcol = country_refcol, value = value
  )
  for (arg_name in names(single_char_args)) {
    arg_value <- single_char_args[[arg_name]]
    if (!inherits(arg_value, "character") || length(arg_value) != 1) {
      stop(paste0("'", arg_name, "' must be a single character string, not ", class(arg_value)))
    }
  }

  df_or_null_args <- list(
    capitals_ref = capitals_ref, centroids_ref = centroids_ref,
    inst_ref = inst_ref, range_ref = range_ref
  )
  for (arg_name in names(df_or_null_args)) {
    arg_value <- df_or_null_args[[arg_name]]
    if (!is.null(arg_value) && !inherits(arg_value, "data.frame")) {
      stop(paste0("'", arg_name, "' must be a data.frame or NULL, not ", class(arg_value)))
    }
  }

  spatial_or_null_args <- list(
    country_ref = country_ref, seas_ref = seas_ref, urban_ref = urban_ref
  )
  for (arg_name in names(spatial_or_null_args)) {
    arg_value <- spatial_or_null_args[[arg_name]]
    if (!is.null(arg_value) && !inherits(arg_value, "SpatVector")) {
      stop(paste0("'", arg_name, "' must be a SpatVector, or NULL, not ", class(arg_value)))
    }
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

  map_old_to_new <- c(
    .summary = "flag_CoordinateCleaner_summary",
    .val = "flag_CoordinateCleaner_val",
    .equ = "flag_CoordinateCleaner_equal",
    .zer = "flag_CoordinateCleaner_zeros",
    .cap = "flag_CoordinateCleaner_capitals",
    .cen = "flag_CoordinateCleaner_centroids",
    .con = "flag_CoordinateCleaner_countries",
    .sea = "flag_CoordinateCleaner_sea",
    .otl = "flag_CoordinateCleaner_outliers",
    .gbf = "flag_CoordinateCleaner_gbif",
    .inst = "flag_CoordinateCleaner_institutions",
    .urb = "flag_CoordinateCleaner_urban",
    .dpl = "flag_CoordinateCleaner_duplicates",
    .aohi = "flag_CoordinateCleaner_aohi",
    .rang = "flag_CoordinateCleaner_range"
  )

  present_old_cols <- names(map_old_to_new) %in% names(res_flag)
  filtered_map_old_to_new <- map_old_to_new[present_old_cols]

  renames_for_splicing <- setNames(names(filtered_map_old_to_new), filtered_map_old_to_new)

    res_flag <- res_flag %>%
    dplyr::rename(!!!renames_for_splicing)

  return(res_flag)
}
