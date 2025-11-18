#' @title flag_fishbase
#'
#' @usage flag_fishbase(data_dir, occ, species = "species",
#' long = "decimalLongitude", lat = "decimalLatitude", buffer = 20,
#' verbose = TRUE)
#'
#' @description
#' Flags (validates) occurrence records against locally stored FishBase data.
#' This function checks if an occurrence point for a given fish species falls
#' within the known country-level distribution polygons associated with that
#' species. A user-defined buffer (in kilometers) can be applied to these
#' polygons.
#'
#' @param data_dir (character) the path to the local directory where the
#' fishbase subdirectory is located. **Required.**
#' @param occ (data.frame) a data frame containing the occurrence records
#' to be flagged. Must include columns for species, longitude, and latitude. **Required.**
#' @param species (character) the name of the column in occ that
#' contains the species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in occ that
#' contains the longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in occ that
#' contains the latitude values. Default is `"decimalLatitude"`.
#' @param buffer (numeric) the buffer distance (in kilometers) to apply
#' around the known country distribution polygons before checking for
#' intersection. Default is 20 km.
#' @param verbose (logical) if `TRUE`, prints messages about progress
#' and the number of species being checked. Default is `TRUE`.
#'
#' @return
#' Returns the original \code{occ} data frame augmented with a new logical column
#' named \code{fishbase_flag}. A value of \code{TRUE} indicates the record falls
#' within the buffered known distribution, \code{FALSE} indicates it falls
#' outside, and \code{NA} indicates the species was not found in the local
#' FishBase data.
#'
#' @importFrom data.table fread rbindlist
#' @importFrom terra vect aggregate buffer is.related
#' @importFrom pbapply pblapply
#' @importFrom dplyr %>% bind_rows
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # NOTE: The directory specified in 'data_dir' must exist and contain the
#' # fishbase data
#' # You must run 'fishbase_here()' beforehand to download the necessary data
#' # files.
#'
#' occ <- RuHere::occ_fish
#'
#' results <- flag_fishbase(
#'   data_dir = "your/path/here",
#'   occ = occ,
#'   species = "species",
#'   long = "decimalLongitude",
#'   lat = "decimalLatitude"
#' )
#' }
#'
flag_fishbase <- function(data_dir, occ, species = "species",
                          long = "decimalLongitude", lat = "decimalLatitude",
                          buffer = 20,
                          verbose = TRUE) {

  if (missing(data_dir) || is.null(data_dir)) {
    stop("'data_dir' is required (must not be NULL or missing).")
  }
  if (!inherits(data_dir, "character")) {
    stop("'data_dir' must be a character, not ", class(data_dir))
  }

  if (missing(occ) || is.null(occ)) {
    stop("'occ' should be specified (must not be NULL or missing).")
  } else if (!inherits(occ, "data.frame")) {
    stop("'occ' should be a data.frame, not ", class(occ))
  }

  if (!inherits(species, "character")) {
    stop("'species' should be a character, not ", class(species))
  }

  if (!inherits(long, "character")) {
    stop("'long' should be a character, not ", class(long))
  }

  if (!inherits(lat, "character")) {
    stop("'lat' should be a character, not ", class(lat))
  }

  if (!inherits(verbose, "logical")) {
    stop("'verbose' must be logical, not ", class(verbose))
  }

  if (!inherits(buffer, "numeric")) {
    stop("'buffer' must be numeric, not ", class(buffer))
  }

  if (!dir.exists(file.path(data_dir, "fishbase/"))) {
    stop("Directory 'fishbase/' not found in ", data_dir,
         ".\\nCheck the folder or run the 'fishbase_here()' function")
  }

  if (!file.exists(file.path(data_dir, "fishbase/fb_species_country.gz"))) {
    stop("Data 'fb_species_country.gz' necessary to check records is not available in ", data_dir,
         ".\\nCheck the folder or run the 'fishbase_here()' function")
  }

  if (!file.exists(file.path(data_dir, "fishbase/fb_countries_decoder.gz"))) {
    stop("Data 'fb_countries_decoder.gz' (decoder) necessary to check records is not available in ", data_dir,
         ".\\nCheck the folder or run the 'fishbase_here()' function")
  }

  all_species <- NULL

  d_country <- data.table::fread(file.path(data_dir, "fishbase/fb_species_country.gz"))
  d_country$country <- tolower(d_country$country)
  all_species <- c(all_species, d_country$Species)

  m_country <- terra::vect(system.file("extdata/world.gpkg", package = "RuHere"))

  d_species <- unique(all_species)
  spp_in <- intersect(unique(occ[[species]]), d_species)
  spp_out <- setdiff(unique(occ[[species]]), d_species)

  if (length(spp_out) > 0) {
    warning("Some species present in occ will not be checked due to absence of information in FishBase")
  }

  if (length(spp_in) == 0) {
    stop("None of the species in occ has information in the FishBase data available")
  }

  if (verbose) {
    message("Checking the distribution of ", length(spp_in), " of ",
            length(unique(occ[[species]])), " species")
  }

  res_flag <- pbapply::pblapply(spp_in, function(i) {

    occ_i <- occ[occ[[species]] == i, ]

    m_i_country <- NULL
    occ_i$fishbase_flag <- NA
    d_i_country <- d_country[d_country$Species == i, ]

    if (nrow(d_i_country) > 0) {
      sp_countries <- unique(d_i_country$country)
      m_country_sub <- m_country[m_country$name %in% sp_countries, ]

      if (nrow(m_country_sub) > 0) {
        m_i_country <- terra::aggregate(m_country_sub)
        m_i_country <- terra::buffer(m_i_country, width = buffer * 1000)
      }
    }

    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    if (!is.null(m_i_country)) {
      occ_i$fishbase_flag <- terra::is.related(pts, m_i_country, "intersects")
    }

    return(occ_i)

  })

  res_flag <- data.table::rbindlist(res_flag, fill = TRUE) %>% as.data.frame()

  if (length(spp_out) > 0) {
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$fishbase_flag <- NA
    res_flag <- dplyr::bind_rows(res_flag, occ_out)
  }

  return(res_flag)
}
