#' Title
#'
#' @param occ
#' @param species
#' @param long
#' @param lat
#' @param d
#' @param prioritary_column
#' @param desc
#'
#' @returns
#' @export
#'
#' @examples
thin_records <- function(occ,
                         species = "species",
                         long = "decimalLongitude",
                         lat = "decimalLatitude",
                         d,
                         prioritary_column = NULL,
                         desc = TRUE){
  if(!is.null(prioritary_column)){
    occ2 <- occ %>%
      dplyr::arrange(dplyr::desc(!!dplyr::sym(prioritary_column))) %>%
      dplyr::mutate(original_order = row_number())
  }

  # Create column to flag
  occ2$thin_flag <- TRUE

  # Convert to spatial points
  pts <- terra::vect(occ2,
                     geom = c(x = long, y = lat),
                     crs = "epsg:4326")

  # Get number of records
  n <- nrow(occ2)

  for (i in 1:n) {
    #print(i)

    # If record has been already removed, skip...
    if (occ2$thin_flag[i]) {
      # Create buffer around point
      b <- pts[i,] %>% terra::buffer(., width = d * 1000)

      # Select other records
      other_records <- which(occ2$thin_flag)
      other_records <- other_records[other_records > i]
      # # Test
      # mapview(pts[i,],col.regions = "red") + mapview(pts) +
      #   mapview(b, col.regions = "yellow")

      # Check if records are inside buffer
      # Flag (FALSE) if is inside
      occ2$thin_flag[other_records] <- !is.related(pts[other_records,], b, "intersects")
    }

  }

  # Original order
  occ <- occ2 %>% dplyr::arrange(original_order) %>%
    dplyr::select(-original_order)

  # # Test
  # pts3 <- terra::vect(occ3, geom = c(x = long, y = lat), crs = "epsg:4326")
  # mapview(pts3, zcol = "thin_flag", burst = TRUE)

  return(occ)
}

# occ <- RuHere::occ_flagged
# occ <- remove_flagged(occ)
# occ_thinned <- thin_records(occ = occ, prioritary_column = "year", d = 15)
# pts3 <- terra::vect(occ_thinned, geom = c(x = long, y = lat), crs = "epsg:4326")
# mapview(pts3, zcol = "thin_flag", burst = TRUE)
