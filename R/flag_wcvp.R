#' Identify records outside natural ranges according to the World Checklist of Vascular Plants
#'
#' @description
#' Flags (validates) occurrence records based on known distribution data
#' from the World Checklist of Vascular Plants (WCVP) data. This function checks
#' if an occurrence point for a given species falls within its documented
#' distribution, allowing for user-defined buffers around the region. Records
#' are flagged as valid (`TRUE`) if they fall inside the documented
#' distribution (plus optional buffer) for the species in the WCVP dataset.
#'
#' @param data_dir (character) **Required** directory path where the `WCVP`
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
#' `"introduced"`, `"extinct"`, `"location_doubtful"`, and `"all"`. For example,
#' if  `origin = "introduced"`, only regions where the species is considered
#' introduced will be used to validate records.  Default is `"native"`.
#' @param buffer (numeric) buffer distance (in kilometers) to be applied
#' around the region of distribution. Default is 20 km.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `FALSE`, prints messages about the progress and
#' the number of species being checked. Default is `FALSE`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{wcvp_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{WCVP}
#' data. Records for species not found in the \code{WCVP} data will have
#' \code{NA} in the \code{wcvp_flag} column.
#'
#' @importFrom data.table fread rbindlist
#' @importFrom terra vect as.data.frame aggregate buffer is.related
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Filter occurrences for Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Set folder where distributional datasets were saved
#' # Here, just a sample provided in the package
#' # You must run 'wcvp_here()' beforehand to download the necessary data files
#' dataset_dir <- system.file("extdata/datasets", package = "RuHere")
#'
#' # Flag records using WCVP specialist information
#' occ_wcvp <- flag_wcvp(data_dir = dataset_dir, occ = occ)
#'
flag_wcvp <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      origin = "native",
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
      stop(sprintf("Column '%s' specified in '%s' is not present in 'occ'.",
                   value, arg), call. = FALSE)
    }
  }

  # origin
  valid_origins <- c("native", "introduced", "extinct", "location_doubtful")
  if (!all(origin %in% valid_origins)) {
    stop(
      sprintf(
        "'origin' contains invalid values. Allowed options are: %s.",
        paste(valid_origins, collapse = ", ")
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
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }


  # Check if data_dir exists
  if(!file.exists(file.path(data_dir, "wcvp/wcvp.gz"))){
    stop("Data from WCVP necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'wcvp_here()' function")
  }
  if(!file.exists(file.path(data_dir, "wcvp/wgsrpd.gpkg"))){
    stop("Data from WCVP necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'wcvp_here()' function")
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
  d <- data.table::fread(file.path(data_dir, "wcvp/wcvp.gz"),
                         data.table = FALSE)
  # Import map
  m <- terra::vect(file.path(data_dir, "wcvp/wgsrpd.gpkg"))
  # Get dataframe to filter levels
  dm <- terra::as.data.frame(m)

  # Get species in data
  spp_in <- intersect(unique(occ[[species]]),
                      unique(d$species))
  spp_out <- setdiff(unique(occ[[species]]),
                     unique(d$species))

  #Warning if some species are not available
  if(length(spp_out) > 0){
    warning("Some species present in occ will not be checked due to absence of information in WCVP")
  }

  if(length(spp_in) == 0){
    stop("None of the species in occ has information in the WCVP data available")
  }

  if(verbose){
    message("Checking the distribution from ", length(spp_in), " of ",
            length(unique(occ[["species"]])), " species")
  }

  res_flag <- my_lapply(spp_in, function(i){
    # Get data from wcvp
    d_i <- d[d$species == i, ]

    # if origin is not NULL...
    if(origin != "all"){
      if(!("native" %in% origin)){
        d_i <- d_i[d_i$introduced == 1 | d_i$extinct == 1 |
                     d_i$location_doubtful == 1,]
      }
      if(!("introduced" %in% origin)){
        d_i <- d_i[d_i$introduced == 0,]
      }
      if(!("extinct" %in% origin)){
        d_i <- d_i[d_i$extinct == 0,]
      }
      if(!("location_doubtful" %in% origin)){
        d_i <- d_i[d_i$location_doubtful == 0,]
      }
    }

    # Subset map
    m_i <- m[m$Level3_cod %in% d_i$LEVEL3_COD]
    m_i <- terra::aggregate(m_i)

    #Add buffer
    if(!is.null(buffer)){
      m_i <- terra::buffer(m_i, width = buffer * 1000)
    }

    # Check if records fall inside
    occ_i <- occ[occ[[species]] == i, ]
    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    occ_i$wcvp_flag <- terra::is.related(pts, m_i, "intersects")

    # # Test
    # plot(m_i)
    # points(pts, col = ifelse(occ_i$wcvp_flag, "green", "red"))

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


