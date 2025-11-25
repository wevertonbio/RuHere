# Rcpp::sourceCpp("src/spatial_kde.cpp")

spatial_kde <- function(occ,
                        long = "decimalLongitude",
                        lat = "decimalLatitude",
                        bandwidth = NULL,
                        resolution = NULL,
                        crs = "epsg:4326",
                        raster_ref = NULL,
                        kernel = "quartic",
                        scaled = TRUE,
                        decay = 1,
                        mask = NULL,
                        weights = numeric(0)) {
  # Convert occ to points
  if(inherits(occ, "data.frame") | inherits(occ, "data.table")){
    pts <- spatialize(occ = occ, long = long, lat = lat, crs = crs)
  } else {
    pts <- pts
  }


  if (!is.null(raster_ref)) {
    # Option A: Use provided raster reference
    r_base <- raster_ref

  } else {
    # Option B: Use bandwidth and resolution to define the grid
    if (bandwidth <= 0 || resolution <= 0) {
      stop("'bandwidth' and 'resolution' must be positive values.")
    }

    # Expand the extent by the bandwidth (analogous to QGIS calculateBounds)
    adjusted_extent <- terra::convHull(pts) %>%
      terra::buffer(width = bandwidth * 1000)

    if(!is.null(mask)){
      adjusted_extent <- terra::crop(adjusted_extent, mask)
      # Select points
      is_inside <- terra::is.related(pts, adjusted_extent, "intersects")
      if(sum(!is_inside) > 0){
        pts <- pts[is_inside,]
        occ <- occ[is_inside, ]
      }
    }

    # Create the empty SpatRaster
    r_base <- terra::rast(ext = terra::ext(adjusted_extent),
                          resolution = resolution,
                          crs = terra::crs(pts))
  }


  # Define fishnet and input matrix
  # Get the centroid coordinates of every cell in the base raster (The Fishnet)
  # This matrix is the input 'fishnet' for the C++ function
  fishnet_matrix <- terra::xyFromCell(r_base, 1:terra::ncell(r_base))

  #Call c++ function
  density_values <- kde_rcpp(
    fishnet = fishnet_matrix,
    points = as.matrix(occ[, c(long, lat)]),
    bw = bandwidth,
    kernel = kernel,
    scaled = scaled,
    decay = decay,
    weights = weights
  )

  # Scaled from 1 to 0?
  if(scaled){
    density_values <- (density_values-min(density_values, na.rm = TRUE))/
      (max(density_values, na.rm = TRUE)-min(density_values, na.rm = TRUE))
  }

  # Assign the calculated density values back to the reference SpatRaster
  terra::values(r_base) <- density_values

  if(!is.null(mask)){
    r_base <- terra::crop(r_base, mask, mask = TRUE)
  }

  return(r_base)
}
#
#
# occ <- RuHere::occurrences
# long = "decimalLongitude"
# lat = "decimalLatitude"
# bandwidth = 0.5
# resolution = 0.041
# raster_ref = NULL
# kernel = "quartic"
# scaled = TRUE
# decay = 1
# weights = numeric(0)
# mask = geobr::read_state("PR") %>% vect()
#
# a <- spatial_kde(occ = occ, bandwidth = 0.4, resolution = 0.041, mask = mask,
#                  scaled = TRUE)
# plot(a)
# points(spatialize(occ))
