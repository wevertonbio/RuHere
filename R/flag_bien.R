#' Identify records outside natural ranges according to BIEN
#'
#' @description
#' Flags (validates) occurrence records based on known distribution data
#' from the Botanical Information and Ecology Network (BIEN) data. This function
#' checks if an occurrence point for a given species falls within its documented
#' distribution, allowing for user-defined buffers around the region. Records
#' are flagged as valid (`TRUE`) if they fall inside the documented
#' distribution (plus optional buffer) for the species in the BIEN dataset.
#'
#' @param data_dir (character) **Required** directory path where the `BIEN`
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
#' @param buffer (numeric) buffer distance (in kilometers) to be applied
#' around the region of distribution. Default is 20 km.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @returns
#' A \code{data.frame} that is the original \code{occ} data frame
#' augmented with a new column named \code{bien_flag}. This column is
#' logical (\code{TRUE}/\code{FALSE}) indicating whether the record falls
#' within the expected distribution (plus buffer) based on the \code{BIEN}
#' data. Records for species not found in the \code{BIEN} data will have
#' \code{NA} in the \code{bien_flag} column.
#'
#' @importFrom terra vect buffer is.related
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Filter occurrences for golden trumpet tree
#' occ <- occurrences[occurrences$species == "Handroanthus serratifolius", ]
#' # Set folder where distributional datasets were saved
#' # Here, just a sample provided in the package
#' # You must run 'bien_here()' beforehand to download the necessary data files
#' dataset_dir <- system.file("extdata/datasets", package = "RuHere")
#'
#' # Flag records using BIEN specialist information
#' occ_bien <- flag_bien(data_dir = dataset_dir, occ = occ)
#'
flag_bien <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      buffer = 10, progress_bar = FALSE, verbose = TRUE){

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

  # buffer
  if (!inherits(buffer, "numeric") || length(buffer) != 1 || buffer < 0) {
    stop("'buffer' must be a single non-negative numeric value.", call. = FALSE)
  }

  # verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)
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

  # Check if data_dir exists
  if(!file.exists(data_dir)){
    stop("Data from BIEN necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'bien_here()' function")
  }


  # Check species with data to filter
  all_spp <- unique(occ[[species]])
  # Name vector replacing space with _
  names(all_spp) <- gsub(" ", "_", all_spp)
  v_files <- list.files(file.path(data_dir, "bien"), pattern = ".gpkg")
  spp_files <- sub(".gpkg", "", v_files)
  spp_in <- intersect(names(all_spp), spp_files)
  spp_out <- setdiff(names(all_spp), spp_files)

  #Warning if some species are not available
  if(length(spp_out) > 0){
    warning("Some species present in occ will not be checked due to absence of information in BIEN")
  }

  if(length(spp_in) == 0){
    stop("None of the species in occ has information in the BIEN data available")
  }

  if(verbose){
    message("Checking the distribution from ", length(spp_in), " of ",
            length(all_spp), " species")
  }

  # Flag
  res_flag <- my_lapply(spp_in, function(i){
    # Get species name
    sp_name <- gsub("_", " ", i)

    # Get data from bien
    spp_path <- paste0(data_dir, "/bien/", i, ".gpkg")
    m_i <- terra::vect(spp_path)

    #Add buffer
    if(!is.null(buffer)){
      m_i <- terra::buffer(m_i, width = buffer * 1000)
    }

    # Check if records fall inside
    occ_i <- occ[occ[[species]] == sp_name, ]
    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    occ_i$bien_flag <- terra::is.related(pts, m_i, "intersects")
    # Flags
    return(occ_i)
  })
  res_flag <- as.data.frame(data.table::rbindlist(res_flag))

  # Append occurrences of spp_out, if exists
  if(length(spp_out) > 0){
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$bien_flag <- NA
    res_flag <- rbind(res_flag, occ_out)
  }
  return(res_flag)
}
