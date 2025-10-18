move_to_closest <- function(occ, long, lat, raster_layer, move_limit_distance,
                            verbose = TRUE) {

  # detecting potential errors
  if (missing(occ)) {
    stop("Argument 'occ' must be defined.")
  }
  if (missing(long)) {
    stop("Argument 'x' must be defined.")
  }
  if (missing(lat)) {
    stop("Argument 'y' must be defined.")
  }
  if (missing(raster_layer)) {
    stop("Argument 'raster_layer' must be defined.")
  }
  if (missing(move_limit_distance)) {
    stop("Argument 'move_limit_distance' must be defined.")
  }
  if (class(raster_layer)[1] != "SpatRaster") {
    stop("'raster_layer' must be of class 'SpatRaster'.")
  }

  # preparing occ
  xy <- occ[, c(long, lat)]
  vals <- terra::extract(raster_layer, xy, ID = FALSE)
  tomove <- which(is.na(vals[, 1]))

  # finding pixels to move in
  if (length(tomove) > 0) {
    xyout <- occ[tomove, ]
    no <- nrow(xyout)

    ## basic conditions to fill in table
    condition <- rep("Correct", nrow(occ))
    distss <- rep(0, nrow(occ))

    ## buffer from out
    limdist <- move_limit_distance * 1000
    xyvec <- terra::vect(xyout, crs = "+proj=longlat",
                         geom = c(long, lat))
    xyvec <- terra::buffer(xyvec, width = limdist)

    ## relevant pixels to move
    raster_layer <- terra::crop(raster_layer, xyvec, mask = TRUE)

    xyras <- as.data.frame(raster_layer, xy = TRUE)[, 1:2]

    if (nrow(xyras) >= 1) {
      dists <- terra::distance(xyout[, c(long, lat)],
                               xyras, lonlat = TRUE)

      # running process
      if (verbose == TRUE) {
        message("Moving occurrences to closest pixels...")
      }

      for (i in 1:no) {
        mindis <- min(dists[i, ])
        if (mindis <= limdist) {
          xyin <- xyras[dists[i, ] == mindis, ]
          occ[tomove[i], long] <- xyin[1, 1]
          occ[tomove[i], lat] <- xyin[1, 2]
          condition[tomove[i]] <- "Moved"
          distss[tomove[i]] <- mindis/1000
        } else {
          condition[tomove[i]] <- "Not_moved"
          distss[tomove[i]] <- mindis/1000
        }
      }

    } else {
      for (i in 1:no) {
        condition[tomove[i]] <- "Not_moved"
        distss[tomove[i]] <- NA
      }

      if (verbose == TRUE) {
        message("Occurrences could not be moved due to long distances.")
      }
    }

    occ <- data.frame(occ, condition = condition, distance_km = distss,
                       initial_lon = xy[, 1], initial_lat = xy[, 2])
  } else {
    if (verbose == TRUE) {
      message("All points are inside valid raster boundaries.")
    }
  }

  return(occ)
}
