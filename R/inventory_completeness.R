#' Estimation of inventory completeness and coverage deficit
#'
#' @description
#' Estimates expected species richness, sample coverage (inventory completeness),
#' and coverage deficit for spatial units based on the framework proposed by
#' Chao & Jost (2012).
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records. Must contain columns for species, longitude, and latitude.
#' @param species (character) the name of the column in occ that contains the
#' species names Default is "species".
#' @param long (character) the name of the column in occ that contains the
#' longitude values. Default is "decimalLongitude".
#' @param lat (character) the name of the column in occ that contains the
#' latitude values. Default is "decimalLatitude".
#' @param raster_base (SpatRaster) a reference raster used to aggregate records
#' into spatial units.
#' @param minimum_species (numeric) the minimum number of species required in a
#' cell to calculate completeness and deficit. If the number of observed species
#' is lower than this threshold, the function sets completeness = 0 and
#' deficit = 1. Default is 3.
#' @param maximum_expected (numeric or character) The upper limit for the
#' estimated species richness (s_exp). Options include:
#' \itemize{
#'   \item \bold{"equal_obs"}: Limits s_exp to the maximum observed richness
#'   (sefault).
#'   \item \bold{"double_obs"}: Limits s_exp to twice the maximum observed
#'   richness found across all cells.
#'   \item \bold{"triple_obs"}: Limits s_exp to three times the maximum observed
#'   richness global.
#'   \item \bold{"free"}: No limit is applied to the Chao1 estimator.
#'   \item \bold{numeric}: A fixed integer defining the maximum number of
#'   species allowed for any cell.
#' }
#' This prevents mathematically inflated estimates in cells with extremely
#' low sampling coverage.
#' @param remove_NA (logical) whether to remove sampling units in raster_base
#' where values are NA.
#' @param fill_NA (logical) if TRUE (default), cells within the `raster_base`
#' without occurrence records are assigned completeness = 0 and deficit = 1. If
#' FALSE, these cells remain NA.
#' @param return (character) metrics to return.. Available options are "n",
#' "s_obs", "s_exp", "singletons", "doubletons", "completeness" and "deficit". See details.
#'
#' @details
#' The function calculates metrics based on the frequency of rare species
#' (singletons and doubletons) within each cell of the `raster_base`.
#' \itemize{
#'   \item \bold{n}: Total number of records.
#'   \item \bold{s_obs}: Observed species richness (number of sampled species).
#'   \item \bold{s_exp}: Estimated asymptotic species richness based on the
#'   Chao1 estimator.
#'   \item \bold{singletons}: Species represented by exactly one record.
#'   \item \bold{doubletons}: Species represented by exactly two records.
#'   \item \bold{completeness}: Sample coverage, representing the proportion of
#'   the total individuals in `occ` that belong to the species in the sample.
#'   \item \bold{deficit}: Coverage deficit, which is the probability that the
#'   next sampled individual represents a previously unsampled species
#'   (1 - completeness)
#' }
#'
#' @returns A SpatRaster object containing the spatialized metrics defined in
#' `return`.
#'
#' @references Chao A, Jost L (2012) Coverage-based rarefaction and extrapolation: standardizing samples by completeness rather than size. Ecology 93:2533–2547. https://doi.org/10.1890/11-1952.1
#'
#' @importFrom terra cellFromXY values rast ncell
#' @importFrom data.table as.data.table uniqueN := .SD
#' @export
#'
#' @examples
#' # Load example of raster variables
#' data("worldclim", package = "RuHere")
#' r <- terra::unwrap(worldclim)
#' # Aggregate cells
#' r_base <- terra::aggregate(r, 5)
#'
#' # Import data set of amphibian communities from the Atlantic Forest
#' data("atlantic_amphibians", package = "RuHere")
#'
#' # Run analysis
#' res <- inventory_completeness(occ = atlantic_amphibians,  raster_base = r_base)
#' terra::plot(res)
#'
inventory_completeness <- function(occ, species = "species",
                                   long = "decimalLongitude",
                                   lat = "decimalLatitude",
                                   raster_base,
                                   minimum_species = 3,
                                   maximum_expected = "equal_obs",
                                   remove_NA = TRUE,
                                   fill_NA = TRUE,
                                   return = c("completeness", "deficit")) {

  #### Start checking ####
  # --- 1. Class and Basic Type Checks ---
  if (!inherits(occ, "data.frame"))
    stop("`occ` must be a data.frame or data.table.", call. = FALSE)

  if (!inherits(raster_base, "SpatRaster"))
    stop("`raster_base` must be a SpatRaster object.", call. = FALSE)

  if (!is.numeric(minimum_species) || length(minimum_species) != 1)
    stop("`minimum_species` must be a single numeric value.", call. = FALSE)

  if (!is.logical(remove_NA) || !is.logical(fill_NA))
    stop("`remove_NA` and `fill_NA` must be logical values (TRUE/FALSE).", call. = FALSE)

  required_cols <- c(species, long, lat)
  missing_cols <- required_cols[!required_cols %in% names(occ)]
  if (length(missing_cols) > 0) {
    stop("The following columns are missing in `occ`: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # Validating 'maximum_expected'
  valid_max_options <- c("equal_obs", "double_obs", "triple_obs", "free")
  if (!is.numeric(maximum_expected)) {
    if (!(maximum_expected %in% valid_max_options)) {
      stop("`maximum_expected` must be numeric or one of: ",
           paste(valid_max_options, collapse = ", "), call. = FALSE)
    }
  }

  # Validating 'return' metrics
  valid_returns <- c("n", "s_obs", "s_exp", "singletons", "doubletons", "completeness", "deficit")
  invalid_returns <- return[!return %in% valid_returns]
  if (length(invalid_returns) > 0) {
    stop("Invalid metrics in `return`: ",
         paste(invalid_returns, collapse = ", "), ". \nSupported options: ",
         paste(valid_returns, collapse = ", "), call. = FALSE)
  }

  #### End of checking ####

  # Set occ as data.table
  dt <- data.table::as.data.table(occ)

  # Get cells
  dt[, cell_id := terra::cellFromXY(raster_base, as.matrix(.SD)), .SDcols = c(long, lat)]

  # Remove NAs (Outside the mask)
  dt <- dt[!is.na(cell_id)]
  if (remove_NA) {
    base_vals <- terra::values(raster_base)
    dt <- dt[!is.na(base_vals[cell_id])]
  }

  # Maximum richness observed
  max_s_obs_global <- max(
    dt[, list(s = data.table::uniqueN(.SD[[1]])),
       by = cell_id,
       .SDcols = species]$s
  )

  # Get maximum expected
  if (is.numeric(maximum_expected)) {
    max_expected <- maximum_expected
  } else if (maximum_expected == "equal_obs") {
    max_expected <- max_s_obs_global
  } else if (maximum_expected == "double_obs") {
    max_expected <- max_s_obs_global * 2
  } else if (maximum_expected == "triple_obs") {
    max_expected <- max_s_obs_global * 3
  }


  # 2. Process sampling units
  res_dt <- dt[, {
    # Extract species column name
    species_vec <- .SD[[1]]

    # Get metrics
    stats <- get_rarefaction_components(species_vec)

    n_val  <- as.numeric(stats["n"])
    s_obs  <- as.numeric(stats["s_obs"])
    f1     <- as.numeric(stats["f1"])
    f2     <- as.numeric(stats["f2"])

    if (s_obs <= minimum_species) {
      s_exp_val <- s_obs
      C_hat <- 0
      def_val <- 1
    } else {
      # Expected richness (Chao1)
      if (f1 > 0 && f2 > 0) {
        s_exp_val <- s_obs + ((n_val - 1) / n_val) * (f1^2 / (2 * f2))
      } else if (f1 > 1 && f2 == 0) {
        # Correction for when there is no doubletons
        s_exp_val <- s_obs + ((n_val - 1) / n_val) * (f1 * (f1 - 1) / 2)
      } else {
        s_exp_val <- s_obs
      }

      # Correction of huge values
      if(maximum_expected != "free"){
        if(s_exp_val > max_expected){
          s_exp_val <- max_expected
          }
      }

      # Sample Coverage (completeness)
      denom <- (n_val - 1) * f1 + 2 * f2
      C_hat <- if (denom > 0) {
        1 - (f1 / n_val) * (((n_val - 1) * f1) / denom)
      } else {
        1 - (f1 / n_val) # Turing estimator
      }

      # Coverage Deficit
      def_val <- 1 - C_hat

    }

    list(
      n = n_val,
      s_obs = s_obs,
      s_exp = as.numeric(s_exp_val),
      singletons = f1,
      doubletons = f2,
      completeness = C_hat,
      deficit = def_val
    )
  }, by = cell_id, .SDcols = species]

  # 3. Build raster
  mapa_final <- terra::rast(raster_base, nlyrs = length(return))
  names(mapa_final) <- return

  for(layer in return) {
    # Criamos o vetor inicial com NAs
    vetor_completo <- rep(NA, terra::ncell(raster_base))

    # If fill NA...
    if (fill_NA) {
      is_land <- !is.na(terra::values(raster_base[[1]])) #cells with values
      if (layer == "completeness") {
        vetor_completo[is_land] <- 0
      } else if (layer == "deficit") {
        vetor_completo[is_land] <- 1
      } else {
        vetor_completo[is_land] <- 0 # n, s_obs, s_exp, etc.
      }
    }

    vetor_completo[res_dt$cell_id] <- res_dt[[layer]]
    terra::values(mapa_final[[layer]]) <- vetor_completo
  }

  return(mapa_final)
}
