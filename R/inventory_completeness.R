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
#' @param maximum_expected (numeric or character) The upper limit for the estimated
#' species richness (s_exp). Options include:
#' \itemize{
#'   \item \bold{"double_obs"}: Limits s_exp to twice the maximum observed richness
#'   found across all cells (recommended for stability).
#'   \item \bold{"triple_obs"}: Limits s_exp to three times the maximum observed
#'   richness global.
#'   \item \bold{"free"}: No limit is applied to the Chao1 estimator.
#'   \item \bold{numeric}: A fixed integer defining the maximum number of species
#'   allowed for any cell.
#' }
#' This prevents mathematically inflated estimates in cells with extremely
#' low sampling coverage. Default is "double_obs".
#' @param remove_NA (logical) whether to remove sampling units in raster_base
#' where values are NA.
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
                                   maximum_expected = "double_obs",
                                   remove_NA = TRUE,
                                   return = c("completeness", "deficit")) {

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
      s_exp = s_exp_val,
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
    vetor_completo <- rep(NA, terra::ncell(raster_base))
    vetor_completo[res_dt$cell_id] <- res_dt[[layer]]
    terra::values(mapa_final[[layer]]) <- vetor_completo
  }

  return(mapa_final)
}

# # Example
# # Load example data
# data("occurrences", package = "RuHere")
# res <- inventory_completeness(occ = occ_flagged, raster_base = raster_base)
# plot(res)
# #
#
#
#
# # # # Import raster base
# af <- vect("../AF_vertebrates/Vectors/AF.gpkg")
# r_base <- rast(af, res = 0.25, vals = 1)
# r_base <- crop(r_base, af, mask = TRUE)
# terra::plot(r_base)
# raster_base = r_base
# # occ <- data.table::fread("../AF_vertebrates/Data/Occurrence_data/Check_Points/J - Cleaned records with CoordinateCleaner.gz",
# #                          select = c("species", "decimalLongitude", "decimalLatitude"), data.table = FALSE)
# # occ2 <- occ
# # species = "species"
# # long = "decimalLongitude"
# # lat = "decimalLatitude"
# # return = c("completeness", "deficit")
# #
# inv <- inventory_completeness(occ = occ2, raster_base = raster_base,
#                               return = c("s_obs", "s_exp", "completeness", "deficit"))
# terra::plot(inv)

# # Testar o método de Défice (Esperamos valores altos de lacuna por causa das espécies raras)
# mapa_teste <- analisar_amostragem(sim_dados, res_km = 100, metodo = "defice")
#
# # Verificar se o resultado é um objeto SpatRaster do terra
# print(mapa_teste)
#
# # Visualizar
# plot(mapa_teste, col = rev(heat.colors(10)), main = "Teste: Défice de Inventário")
# points(sim_dados$longitude, sim_dados$latitude, pch = 20, cex = 0.5)
#
#
# z <- analisar_amostragem(d, res_km = 100, metodo = "defice")
