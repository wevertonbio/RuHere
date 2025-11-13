#' Title
#'
#' @param data_dir
#' @param occ
#' @param species
#' @param long
#' @param lat
#' @param origin
#' @param presence
#' @param buffer
#' @param verbose
#'
#' @returns
#' @export
#'
#' @examples
flag_bien <- function(data_dir, occ, species = "species",
                      long = "decimalLongitude", lat = "decimalLatitude",
                      origin = NULL, presence = NULL,
                      buffer = 10, verbose = TRUE){

  # Check if data_dir exists
  if(!file.exists(data_dir)){
    stop("Data from BIEN necessary to check records is not available in ", data_dir,
         ".\nCheck the folder or run the 'bien_here()' function")
  }


  # Check species with data to filter
  all_spp <- unique(occ[[species]])
  v_files <- list.files(file.path(data_dir, "bien"), pattern = ".gpkg")
  spp_files <- sub(".gpkg", "", v_files)
  spp_in <- intersect(all_spp, spp_files)
  spp_out <- setdiff(all_spp, spp_files)

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
  res_flag <- pbapply::pblapply(spp_in, function(i){
    # Get data from bien
    spp_path <- paste0(data_dir, "/bien/", i, ".gpkg")
    m_i <- terra::vect(spp_path)

    #Add buffer
    if(!is.null(buffer)){
      m_i <- terra::buffer(m_i, width = buffer * 1000)
    }

    # Check if records fall inside
    occ_i <- occ[occ[[species]] == i, ]
    pts <- terra::vect(occ_i, geom = c(x = long, y = lat), crs = "epsg:4326")

    occ_i$bien_flag <- terra::is.related(pts, m_i, "intersects")
    # Flags
    return(occ_i)
  })
  res_flag <- data.table::rbindlist(res_flag) %>% as.data.frame()

  # Append occurrences of spp_out, if exists
  if(length(spp_out) > 0){
    occ_out <- occ[occ[[species]] %in% spp_out, ]
    occ_out$bien_flag <- NA
    res_flag <- dplyr::bind_rows(res_flag, occ_out)
  }
  return(res_flag)
}

# data_dir = "../RuHere_test/"
# occ = RuHere::occurrences
# occ$species <- "Paubrasilia echinata"
# species = "species"
# long = "decimalLongitude"
# lat = "decimalLatitude"
# origin = NULL
# presence = NULL
# buffer = 10
# verbose = TRUE
