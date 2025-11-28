#' Kernel Density Estimation (Heatmap) for occurrence data
#'
#' @description
#' This function creates density heatmaps using kernel density estimation.
#' The algorithm is inspired by the **SpatialKDE** R package and the "Heatmap"
#' tool from QGIS.
#' Each occurrence contributes to the density surface within a circular
#' neighborhood defined by a specified radius.
#'
#' @param occ (data.frame, data.table, or SpatVector) a data frame or SpatVector containing the occurrences. Must contain columns longitude and latitude.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param radius (numeric) a positive numeric value specifying the smoothing
#' radius for the kernel density estimate. This parameter determines the
#' circular neighborhood around each point where that point will have an
#' influence. See details. Default is 0.2.
#' @param resolution (numeric) a positive numeric value specifying the resolution
#' (in degrees or meters, depending on the `crs` provided) of the resultant
#' raster heatmap. Required if `raster_ref` is not provided. Default is NULL.
#' @param buffer_extent (numeric) width of the buffer (in kilometers) to draw
#' around the occurrences to define the area for computing the heatmap. Default
#' is 500.
#' @param crs (character) the coordinate reference system of the raster heatmap
#' (see ?terra::crs). Default is "epsg:4326". Only applicable if `raster_ref`
#' is not provided.
#' @param raster_ref (SpatRaster) an optional  raster to use as reference for
#' resolution, CRS, and extent. Default is NULL.
#' @param kernel (character) type of kernel to use. Available kernerls are
#' "uniform", "quartic", "triweight", "epanechnikov", or "triangular". Default
#' is "quartic".
#' @param scaled (logical) whether to scale output values to vary between 0 and
#' 1. Default is TRUE.
#' @param decay (numeric) decay parameter for "triangular" kernel. Only
#' applicable if `kernel = triangular`.
#' @param mask (SpatRaster or SpatExtent) optional spatial object to define the
#' extent of the area for the heatmap. Default is NULL, in which case the
#' extent is derived from `raster_ref` (if provided) or from the convex hull of
#' occurrences plus `buffer_extent`.
#' @param weights (numeric) optional vector of weights for individual points.
#' Must be the same length as the number of occurrences in `occ`. Default is
#' NULL.
#'
#' @details
#' The `radius` parameter controls how far the influence of each observation
#' extends. Smaller values produce fine-grained peaks; larger values produce
#' smoother, more spread-out heatmaps. Units depend on the CRS: degrees for
#' geographic coordinates (default), meters for projected coordinates.
#'
#' If `raster_ref` is not provided, the extent is calculated from the convex
#' hull of `occ` plus `buffer_extent`.
#'
#' Kernels define the weight decay of points:
#' "uniform" = constant, "quartic"/"triweight"/"epanechnikov" = smooth, and
#' "triangular" = linear decay (using `decay` parameter).
#'
#' @returns
#' A SpatRaster containing the kernel density values.
#'
#' @importFrom terra buffer crop is.related rast ext crs xyFromCell values
#' @export
#'
#' @references
#' * Hart, T., & Zandbergen, P. (2014). Kernel density estimation and hotspot
#'  mapping: Examining the influence of interpolation method, grid cell size,
#'  and radius on crime forecasting. **Policing: An International Journal of**
#'  **Police Strategies & Management**, 37(2), 305-323.
#' * Nelson, T. A., & Boots, B. (2008). Detecting spatial hot spots in landscape
#'  ecology. **Ecography**, 31(5), 556-566.
#' * Chainey, S., Tompson, L., & Uhlig, S. (2008). The utility of hotspot mapping
#'  for predicting spatial patterns of crime. **Security journal**, 21(1), 4-28.
#' * Caha J (2023). SpatialKDE: Kernel Density Estimation for Spatial Data.
#' **https://jancaha.github.io/SpatialKDE/index.html**.
#'
#' @examples
#' # Load example data
#' data("occ_flagged", package = "RuHere")
#' # Remove flagged records
#' occ <- remove_flagged(occ_flagged)
#' # Generate heatmap
#' heatmap <- spatial_kde(occ = occ, resolution = 0.25, buffer_extent = 50,
#'                        radius = 2)
#' # Plot heatmap
#' terra::plot(heatmap)
#' # Add points to the plot
#' pts <- spatialize(occ)
#' terra::points(pts, pch = 1, cex = 0.4)
spatial_kde <- function(occ,
                        long = "decimalLongitude",
                        lat = "decimalLatitude",
                        radius = 0.2,
                        resolution = NULL,
                        buffer_extent = 500,
                        crs = "epsg:4326",
                        raster_ref = NULL,
                        kernel = "quartic",
                        scaled = TRUE,
                        decay = 1,
                        mask = NULL,
                        weights = NULL) {
  #==============================#
  # ARGUMENT CHECKING            #
  #==============================#

  # occ
  if (!inherits(occ, "data.frame") && !inherits(occ, "data.table") && !inherits(occ, "SpatVector")) {
    stop("`occ` must be a data.frame, data.table, or SpatVector.",
         call. = FALSE)
  }

  # Convert occ to points
  if(!inherits(occ, "SpatVector")){
    pts <- spatialize(occ = occ, long = long, lat = lat, crs = crs)
  } else {
    pts <- occ
    occ <- terra::geom(pts)[, c("x", "y")]
    names(occ) <- c(long, lat)
  }

  # long and lat
  if (!inherits(long, "character") || length(long) != 1) {
    stop("`long` must be a single character string.", call. = FALSE)
  }
  if (!inherits(lat, "character") || length(lat) != 1) {
    stop("`lat` must be a single character string.", call. = FALSE)
  }

  if(!inherits(occ, "SpatVector")){
    if (!(long %in% colnames(occ))) {
      stop(paste0("Column '", long, "' not found in `occ`."), call. = FALSE)
    }

    if (!(lat %in% colnames(occ))) {
      stop(paste0("Column '", lat, "' not found in `occ`."), call. = FALSE)
    }
  }

  if(is.null(raster_ref) && (is.null(resolution) || is.null(crs) ||
                             is.null(buffer_extent))){
    stop("If 'raster_ref' is not provided, you must specify 'resolution', 'crs' and 'buffer_extent'")
  }

  # radius
  if (!inherits(radius, "numeric") || length(radius) != 1 || radius <= 0) {
    stop("`radius` must be a single positive numeric value.", call. = FALSE)
  }

  # resolution
  if (!is.null(resolution) && (!inherits(resolution, "numeric") ||
                               length(resolution) != 1 || resolution <= 0)) {
    stop("`resolution` must be a single positive numeric value.", call. = FALSE)
  }

  # buffer_extent
  if (!inherits(buffer_extent, "numeric") || length(buffer_extent) != 1 ||
      buffer_extent < 0) {
    stop("`buffer_extent` must be a single non-negative numeric value.",
         call. = FALSE)
  }

  # crs
  if (!inherits(crs, "character") || length(crs) != 1) {
    stop("`crs` must be a single character string.", call. = FALSE)
  }

  # raster_ref
  if (!is.null(raster_ref) && !inherits(raster_ref, "SpatRaster")) {
    stop("`raster_ref` must be a SpatRaster object or NULL.", call. = FALSE)
  }

  # kernel
  kernel_allowed <- c("uniform", "quartic", "triweight", "epanechnikov",
                      "triangular")
  if (!inherits(kernel, "character") || length(kernel) != 1 || !(kernel %in% kernel_allowed)) {
    stop("`kernel` must be one of: 'uniform', 'quartic', 'triweight', 'epanechnikov', 'triangular'.", call. = FALSE)
  }

  # scaled
  if (!inherits(scaled, "logical") || length(scaled) != 1) {
    stop("`scaled` must be a single logical value.", call. = FALSE)
  }

  # decay
  if (!is.null(decay) && (!inherits(decay, "numeric") || length(decay) != 1 ||
                          decay <= 0)) {
    stop("`decay` must be a single positive numeric value.", call. = FALSE)
  }

  # mask
  if (!is.null(mask) && !(inherits(mask, "SpatRaster") ||
                          inherits(mask, "SpatExtent"))) {
    stop("`mask` must be a SpatRaster, SpatExtent, or NULL.", call. = FALSE)
  }

  # weights
  if (!is.null(weights) && (!inherits(weights, "numeric") ||
                            length(weights) != nrow(occ))) {
    stop("`weights` must be a numeric vector with length equal to nrow(occ) or NULL.", call. = FALSE)
  }

  if (!is.null(raster_ref)) {
    # Option A: Use provided raster reference
    r_base <- raster_ref
    if(!is.null(mask)){
      r_base <- terra::crop(r_base, mask, mask = TRUE)
    }

  } else {
    # Option B: Use radius and resolution to define the grid
    if (radius <= 0 || resolution <= 0) {
      stop("'radius' and 'resolution' must be positive values.")
    }

    # Expand the extent by the buffer_extent
    adjusted_extent <- terra::buffer(terra::convHull(pts),
                                     width = buffer_extent * 1000)
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
                          crs = terra::crs(pts), vals = 0)
  }

  # Crop
  if(!is.null(mask)){
    r_base <- terra::crop(r_base, mask, mask = TRUE)
  } else {
    r_base <- terra::crop(r_base, adjusted_extent, mask = TRUE)
  }

  # Define fishnet and input matrix
  # Get the centroid coordinates of every cell in the base raster (The Fishnet)
  # This matrix is the input 'fishnet' for the C++ function
  not_na <- which(!is.na(terra::values(r_base)))
  fishnet_matrix <- terra::xyFromCell(r_base, not_na)

  # Set weights
  if(is.null(weights)){
    weights <- numeric(0)
  }

  #Call c++ function
  density_values <- kde_rcpp(
    fishnet = fishnet_matrix,
    points = as.matrix(occ[, c(long, lat)]),
    bw = radius,
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
  r_base[not_na] <- density_values

  if(!is.null(mask)){
    r_base <- terra::crop(r_base, mask, mask = TRUE)
  }
  return(r_base)
}
