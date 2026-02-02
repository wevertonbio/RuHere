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
#' @param records (character) the name of the column in `occ` that contains the
#' record names. Default is `"record_id"`.
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
#' @param field (character or named vector) column in `occ` to summarize
#' (e.g., traits). If a named vector is provided, names must match species
#' in `occ`. Used to summarize traits or flags in both 'species' and 'records'
#' modes. Default is `NULL`.
#' @param field_name (character) a custom name used to build the legend when
#' plotting the result with `ggrid_here()`. Only applicable if `field` is
#' provided. Default is NULL, meaning it will use the same column name provided
#' in `field`.
#' @param fun (function) the function to aggregate `field` values
#' (e.g., `mean`, `max`, `sum`). Default is `mean`.
#' @param verbose (logical) whether to print messages about the progress.
#' Default is `TRUE`.
#'
#' @return A `SpatRaster` object representing the calculated richness,
#' density, or trait summary.
#'
#' @importFrom terra rast vect project crs ext rasterize geom cellFromXY xyFromCell mask crop
#'
#' @export
#'
#' @examples
#'# Load example data
#'data("occ_flagged", package = "RuHere")
#'
#'# Mapping the density of records
#'r_density <- richness_here(occ_flagged, summary = "records", res = 0.5)
#'ggrid_here(r_density)
#'
#'# We can also summarize key features:
#'# 1. Identifying problematic regions by summing error flags
#'# We create a variable to store the sum of logical flags (TRUE = 1, FALSE = 0)
#'total_flags <- occ_flagged$florabr_flag +
#'  occ_flagged$wcvp_flag +
#'  occ_flagged$iucn_flag +
#'  occ_flagged$cultivated_flag +
#'  occ_flagged$inaturalist_flag +
#'  occ_flagged$duplicated_flag
#'names(total_flags) <- occ_flagged$record_id
#'
#'# Using summary = "records" with to see the average accumulation of errors
#'# with fun = mean to see the average accumulation
#'r_flags <- richness_here(occ_flagged, summary = "records",
#'                         field = total_flags,
#'                         field_name = "Number of flags",
#'                         fun = mean, res = 0.5)
#'ggrid_here(r_flags)
#'
#'# 2. Or we can summarize organisms traits spatially
#'# Simulating a trait (e.g., mass) for each unique record
#'spp <- unique(occ_flagged$record_id)
#'sim_mass <- setNames(runif(length(spp), 10, 50), spp)
#'
#'r_trait <- richness_here(occ_flagged, summary = "records",
#'                         field = sim_mass, field_name = "Mass",
#'                         fun = mean, res = 0.5)
#'ggrid_here(r_trait)
richness_here <- function(occ, species = "species", long = "decimalLongitude",
                          lat = "decimalLatitude", records = "record_id",
                          raster_base = NULL, res = NULL, crs = "epsg:4326",
                          mask = NULL, summary = "records", field = NULL,
                          field_name = NULL,
                          fun = mean, verbose = TRUE) {

  if (!inherits(occ, "data.frame")) stop("`occ` must be a data.frame.", call. = FALSE)
  if (!is.null(species) && !inherits(species, "character"))
    stop("`species` must be a character vector.", call. = FALSE)
  if (!is.null(records) && !inherits(records, "character"))
    stop("`records` must be a character vector.", call. = FALSE)
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

  required_cols <- c(long, lat)
  if (!all(required_cols %in% names(occ))) {
    missing <- required_cols[!required_cols %in% names(occ)]
    stop("The following columns are missing in `occ`: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  if (!(species %in% names(occ)) && !(records %in% names(occ))) {
    stop("The dataset must contain at least a '", species, "' column or a '", records, "' column.",
         call. = FALSE)
  }


  occ <- occ[!is.na(occ[[long]]) & !is.na(occ[[lat]]), ]
  occ <- as.data.frame(occ)

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

    trait_cols <- NULL
    occ_internal <- occ

    if (!is.null(field)) {
      if (!is.null(names(field))) {
        rec_in_df <- unique(occ_internal[[records]])
        if (!all(rec_in_df %in% names(field))) {
          if(verbose)
            warning("Some records in `occ` do not have a match in the `field` names. Values will be set to NA.")
        }
        occ_internal$trait_val <- field[occ_internal[[records]]]
        trait_cols <- "trait_val"
      } else {
        if (!all(field %in% names(occ_internal))) {
          missing_f <- field[!field %in% names(occ_internal)]
          stop("The following `field` columns are missing in `occ`: ",
               paste(missing_f, collapse = ", "), call. = FALSE)
        }
        trait_cols <- field
      }

      if(is.null(field_name)){
        field_name <- field
        }
    }

    occ_coords <- terra::geom(pts)[, c("x", "y")]
    cell_ids <- terra::cellFromXY(r_template, occ_coords)

    temp_data <- data.frame(cell_id = cell_ids, records = occ_internal[[records]])

    if (!is.null(trait_cols)) temp_data <- cbind(temp_data,
                                                 occ_internal[, trait_cols,
                                                              drop = FALSE])

    occ_unique <- unique(temp_data)
    pts_unique <- terra::vect(terra::xyFromCell(r_template,
                                                occ_unique$cell_id), crs = crs)

    if (is.null(field)) {
      r_final <- terra::rasterize(pts_unique, r_template, fun = length)
      names(r_final) <- "n_records"
    } else {
      for(col in trait_cols) pts_unique[[col]] <- occ_unique[[col]]
      r_final <- terra::rasterize(pts_unique, r_template, field = trait_cols,
                                  fun = fun, na.rm = TRUE)
      names(r_final) <- field_name
    }

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
