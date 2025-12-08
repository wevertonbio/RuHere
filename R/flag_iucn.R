#' Identify records outside natural ranges according to the IUCN
#'
#' @description
#' Flags (validates) occurrence records based on known distribution data
#' from the International Union for Conservation of Nature (IUCN) data. This
#' function checks if an occurrence point for a given species falls within its
#' documented distribution, allowing for user-defined buffers around the region.
#' Records are flagged as valid (`TRUE`) if they fall inside the documented
#' distribution (plus optional buffer) for the species in the IUCN dataset.
#'
#' @param data_dir (character) **Required** directory path where the `IUCN`
#' data is saved
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records to be flagged. Must contain columns for species, longitude, and
#' latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param origin (character) vector specifying which origin categories should
#' be considered as part of the species' range. Options are: `"native"`,
#' `"introduced"`, `"reintroduced"`, `"vagrant"`, `"origin uncertain"`,
#' `"assisted colonisation"`, and `"all"`. For example,
#' if  `origin = "introduced"`, only regions where the species is considered
#' introduced will be used to validate records.  Default is `"native"`.
#' @param presence (character) vector specifying which presence type should
#' be considered as part of the species' range. Options are: `"extant"`,
#' `"probably extant"`, `"possibly extant"`, `"extinct"`, `"possibly extinct"`,
#' `"presence uncertain"`, and `"all"`. For example,
#' if  `presence = "extinct"`, only regions where the species is considered
#' extinct will be used to validate records.  Default is `"all"`, meaning all
#' regions will be considered.
#' @param buffer (numeric) buffer distance (in kilometers) to be applied
#' around the region of distribution. Default is 20 km.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `FALSE`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{iucn_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{IUCN}
#' data. Records for species not found in the \code{IUCN} data will have
#' \code{NA} in the \code{iucn_flag} column.
#'
#' @importFrom data.table fread rbindlist
#' @importFrom terra vect as.data.frame aggregate buffer is.related
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Get only occurrences from Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Set folder where distributional datasets were saved
#' # Here, just a sample provided in tha package
#' # You must run 'iucn_here()' beforehand to download the necessary data files
#' dataset_dir <- system.file("extdata/datasets", package = "RuHere")
#'
#' # Flag records using IUCN specialist information
#' occ_iucn <- flag_iucn(data_dir = dataset_dir, occ = occ)
#'
flag_iucn <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      origin = "native", presence = "all",
                      buffer = 20, progress_bar = FALSE, verbose = FALSE){

  ### --- Argument checking ----------------------------------------------------

  # data_dir
  if (!inherits(data_dir, "character") || length(data_dir) != 1) {
    stop("'data_dir' must be a single character string.", call. = FALSE)
  }
  if (!dir.exists(data_dir)) {
    stop("The directory provided in 'data_dir' does not exist.", call. = FALSE)
  }

  # occ
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table.", call. = FALSE)
  }

  # species, long, lat
  for (arg in c("species", "long", "lat")) {
    value <- get(arg)

    if (!inherits(value, "character") || length(value) != 1) {
      stop(sprintf("'%s' must be a single character string.", arg),
           call. = FALSE)
    }

    if (!value %in% names(occ)) {
      stop(
        sprintf("Column '%s' specified in '%s' is not present in 'occ'.",
                value, arg),
        call. = FALSE
      )
    }
  }

  # origin — must be a character vector with allowed categories
  allowed_origin <- c("native", "introduced", "reintroduced", "vagrant",
                      "origin uncertain", "assisted colonisation", "all")

  if (!inherits(origin, "character")) {
    stop("'origin' must be a character vector.", call. = FALSE)
  }
  if (!all(origin %in% allowed_origin)) {
    stop(
      paste0(
        "'origin' contains invalid values. Allowed categories are: ",
        paste(allowed_origin, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  # presence — must be a character vector with allowed categories
  allowed_presence <- c("extant", "probably extant", "possibly extant",
                        "extinct", "possibly extinct", "presence uncertain",
                        "all")

  if (!inherits(presence, "character")) {
    stop("'presence' must be a character vector.", call. = FALSE)
  }
  if (!all(presence %in% allowed_presence)) {
    stop(
      paste0(
        "'presence' contains invalid values. Allowed categories are: ",
        paste(allowed_presence, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  # buffer
  if (!inherits(buffer, "numeric") || length(buffer) != 1 || buffer < 0) {
    stop("'buffer' must be a single non-negative numeric value.", call. = FALSE)
  }

  # verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }


  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "iucn/iucn_distribution.gz"))){
    stop("Data from IUCN necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'iucn_here()' function")
  }
  if(!file.exists(file.path(data_dir, "iucn/wgsrpd.gpkg"))){
    stop("Data from IUCN necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'iucn_here()' function")
  }

  # progress_bar
  if (!inherits(progress_bar, "logical") || length(progress_bar) != 1) {
    stop("'progress_bar' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  if (progress_bar) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      my_lapply <- pbapply::pblapply
    } else {
      stop("Package 'pbapply' is required if 'progress_bar = TRUE'.
Run install.packages('pbapply')", call. = FALSE)
    }
  } else {
    my_lapply <- base::lapply
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # Import data
  d <- data.table::fread(file.path(data_dir, "iucn/iucn_distribution.gz"),
                         data.table = FALSE)
  # Import map
  m <- terra::vect(file.path(data_dir, "iucn/wgsrpd.gpkg"))
  # Get dataframe to filter levels
  dm <- terra::as.data.frame(m)

  # Get species in data
  spp_in <- intersect(unique(occ[["species"]]),
                      unique(d$species))
  spp_out <- setdiff(unique(occ[["species"]]),
                       unique(d$species))

  #Warning if some species are not available
  if(length(spp_out) > 0){
    warning("Some species present in occ will not be checked due to absence of information in IUCN")
  }

  if(length(spp_in) == 0){
    stop("None of the species in occ has information in the IUCN data available")
  }

  if(verbose){
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique(occ[["species"]])), " species")
  }

  # Flag
  res_flag <- my_lapply(spp_in, function(i){
    # Get data from iucn
    d_i <- d[d$species == i, ]
    # Filter by origin and presence
    if(origin != "all"){
      d_i$origin <- tolower(d_i$origin)
      d_i <- d_i[d_i$origin %in% origin, ]
    }

    if(presence != "all"){
      d_i$presence <- tolower(d_i$presence)
      d_i <- d_i[d_i$presence %in% presence, ]
    }

    # Get regions of occurrence (by level)
    country_level <- d_i$code[nchar(d_i$code) == 2]
    level3 <- d_i$code[nchar(d_i$code) == 3]
    countries_level3 <- dm$ISO_Code[dm$Level3_cod %in% level3]
    level4 <- d_i$code[nchar(d_i$code) > 3]
    countries_level4 <- dm$ISO_Code[dm$Level4_cod %in% level4]

    # Filter levels
    # Remove countries already in levels 3 or 4
    country_level[country_level %in% countries_level3 |
                    country_level %in% countries_level4] <- NA
    country_level <- na.omit(country_level)

    # Remove levels 3 inside level 4
    level3[countries_level3 %in% countries_level4] <- NA
    level3 <- na.omit(level3)

    # Subset map
    m_i <- m[m$ISO_Code %in% country_level |
               m$Level3_cod %in% level3 |
               m$Level4_cod %in% level4, ]
    m_i <- terra::aggregate(m_i)

    #Add buffer
    if(!is.null(buffer)){
      m_i <- terra::buffer(m_i, width = buffer * 1000)
    }

    # Check if records fall inside
    occ_i <- occ[occ[[species]] == i, ]
    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    occ_i$iucn_flag <- terra::is.related(pts, m_i, "intersects")
    # Flags
    return(occ_i)
    })
  res_flag <- as.data.frame(data.table::rbindlist(res_flag))

  # Append occurrences of spp_out, if exists
  if(length(spp_out) > 0){
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$iucn_flag <- NA
    res_flag <- rbind(res_flag, occ_out)
    }
  return(res_flag)
}
