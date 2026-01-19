#' Species Richness and Occurrence Summary Mapping
#'
#' @description
#' This function generates spatial grids (rasters) of species richness,
#' record density, or summarized biological traits from occurrence data.
#' It supports custom resolutions, masking, and automatic coordinate
#' reprojection to match reference rasters.
#'
#' @param occ (data.frame) a dataset containing occurrence records. Must
#' include columns for species names and geographic coordinates.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param raster_base (SpatRaster) an optional reference raster. If provided,
#' the output will match its resolution, extent, and CRS. Default is `NULL`.
#' @param res (numeric) the desired resolution (in decimal degrees if WGS84)
#' for the output grid. Only used if `raster_base` is `NULL`.
#' @param crs (character) the coordinate reference system of the raster.
#' (see ?terra::crs). Default is "epsg:4326". Only applicable if `raster_base`
#' is not provided.
#' @param mask (SpatRaster or SpatVector) an optional layer to mask the
#' final output. Default is `NULL`.
#' @param summary (character) the type of summary to calculate.
#' Either `"records"` (number of occurrences per cell) or `"species"`
#' (number of unique species per cell). Default is `"records"`.
#' @param field (character or named vector) columns in `occ` to summarize
#' (e.g., traits). If a named vector is provided, names must match species
#' in `occ`. Only used when `summary = "species"`. Default is `NULL`.
#' @param fun (function) the function to aggregate `field` values
#' (e.g., `mean`, `max`, `sum`). Default is `mean`.
#' @param verbose (logical) whether to print messages about the progress.
#' Default is `TRUE`
#'
#' @return A `SpatRaster` object representing the calculated richness,
#' density, or trait summary.
#'
#' @importFrom terra rast vect project crs ext rasterize geom cellFromXY xyFromCell mask crop
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' occ <- occurrences
#'
#' # Record density map
#' r_records <- richness_here(occ, res = 0.5, summary = "records")
#' terra::plot(r_records)
#'
#' # Species richness map masked by Brazil's border
#' world <- terra::unwrap(RuHere::world) # Import world map
#' brazil <- world[world$name == "brazil",] # Subset Brazil
#' r_richness <- richness_here(occ, res = 1, mask = brazil, summary = "species")
#' terra::plot(r_richness)
#'
#' # Average trait value per cell
#' sim_mass <- c(runif(length(unique(occ$species)), 10, 20))
#' names(sim_mass) <- unique(occ$species)
#'
#' r_trait <- richness_here(occ, res = 0.5, summary = "species", mask = brazil,
#'                          field = sim_mass, fun = mean)
#' terra::plot(r_trait)
#'
richness_here <- function(occ, species = "species", long = "decimalLongitude",
                          lat = "decimalLatitude", raster_base = NULL,
                          res = NULL, crs = "epsg:4326",
                          mask = NULL, summary = "records",
                          field = NULL, fun = mean, verbose = TRUE) {

  if (!inherits(occ, "data.frame")) stop("`occ` must be a data.frame.", call. = FALSE)
  if (!is.null(species) && !inherits(species, "character"))
    stop("`species` must be a character vector.", call. = FALSE)
  if (!inherits(long, "character"))
    stop("`long` must be a character vector.", call. = FALSE)
  if (!inherits(lat, "character"))
    stop("`lat` must be a character vector.", call. = FALSE)
  if (!inherits(summary, "character") || !summary %in% c("records", "species"))
    stop("`summary` must be either 'records' or 'species'.", call. = FALSE)
  if (!inherits(fun, "function"))
    stop("`fun` must be a function.", call. = FALSE)
  if (!is.null(res) && !inherits(res, c("numeric", "integer")))
    stop("`res` must be a numeric value.", call. = FALSE)
  if (!is.null(raster_base) && !inherits(raster_base, "SpatRaster"))
    stop("`raster_base` must be a SpatRaster object.", call. = FALSE)
  if (!is.null(mask) && !inherits(mask, c("SpatRaster", "SpatVector")))
    stop("`mask` must be a SpatRaster or SpatVector.", call. = FALSE)
  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  # crs
  if (!inherits(crs, "character") || length(crs) != 1) {
    stop("`crs` must be a single character string.", call. = FALSE)
  }


  required_cols <- c(species, long, lat)
  if (!all(required_cols %in% names(occ))) {
    missing <- required_cols[!required_cols %in% names(occ)]
    stop("The following columns are missing in `occ`: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  occ <- occ[!is.na(occ[[long]]) & !is.na(occ[[lat]]), ]

  if (!is.null(raster_base)) {
    r_template <- raster_base
  } else {
    if (is.null(res))
      stop("You must provide either `raster_base` or a `res` value.",
           call. = FALSE)
    ext_pts <- terra::ext(min(occ[[long]]), max(occ[[long]]),
                          min(occ[[lat]]), max(occ[[lat]]))
    r_template <- terra::rast(ext_pts, res = res, crs = crs)
  }

  pts <- terra::vect(occ, geom = c(long, lat), crs = crs)

  # if (terra::crs(pts) != target_crs) {
  #   pts <- terra::project(pts, target_crs)
  # }

  if (summary == "records") {
    r_final <- terra::rasterize(pts, r_template, fun = length)
    names(r_final) <- "n_records"

  } else if (summary == "species") {

    trait_cols <- NULL
    occ_internal <- occ

    if (!is.null(field)) {
      if (!is.null(names(field))) {
        spp_in_df <- unique(occ_internal[[species]])
        if (!all(spp_in_df %in% names(field))) {
          if(verbose)
            warning("Some species in `occ` do not have a match in the `field` names. Values will be set to NA.")
        }
        occ_internal$trait_val <- field[occ_internal[[species]]]
        trait_cols <- "trait_val"
      } else {
        if (!all(field %in% names(occ_internal))) {
          missing_f <- field[!field %in% names(occ_internal)]
          stop("The following `field` columns are missing in `occ`: ",
               paste(missing_f, collapse = ", "), call. = FALSE)
        }
        trait_cols <- field
      }
    }

    occ_coords <- terra::geom(pts)[, c("x", "y")]
    cell_ids <- terra::cellFromXY(r_template, occ_coords)

    temp_data <- data.frame(cell_id = cell_ids,
                            species = occ_internal[[species]])
    if (!is.null(trait_cols)) temp_data <- cbind(temp_data,
                                                 occ_internal[, trait_cols,
                                                              drop = FALSE])

    occ_unique <- unique(temp_data)
    pts_unique <- terra::vect(terra::xyFromCell(r_template,
                                                occ_unique$cell_id), crs = crs)

    if (is.null(field)) {
      r_final <- terra::rasterize(pts_unique, r_template, fun = length)
      names(r_final) <- "richness"
    } else {
      for(col in trait_cols) pts_unique[[col]] <- occ_unique[[col]]
      r_final <- terra::rasterize(pts_unique, r_template, field = trait_cols,
                                  fun = fun, na.rm = TRUE)
      names(r_final) <- trait_cols
    }
  }

  if (!is.null(mask)) {
    if (terra::crs(mask) != terra::crs(r_final)) {
      mask <- terra::project(mask, terra::crs(r_final))
    }
    r_final <- terra::crop(r_final, mask, mask = TRUE)
  }

  return(r_final)
}
