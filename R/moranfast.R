#' Fast Moran's I Autocorrelation Index
#'
#' @description
#' This function computes Moran's I autocorrelation coefficient for a numeric
#' vector `x` using a matrix of weights. The method follows Gittleman and Kot
#' (1990). This function is an implementation of `ape::Moran.I()`, but rewritten
#' in C++ to be substantially faster and more memory-efficient.
#'
#'
#' @param x (numeric) A numeric vector (e.g., environmental values extracted
#' from occurrence records).
#' @param weight (matrix) A matrix of spatial weights (e.g., a distance or
#' inverse-distance matrix). The number of rows must be equal to the length of
#' `x`.
#' @param na_rm (logical) whether to remove missing values from `x`. Default is
#' `TRUE`.
#' @param scaled (logical) whether to scale Moran's I so that it ranges between
#' –1 and +1. Default is `TRUE`.
#' @param alternative (character) The alternative hypothesis tested against
#' the null hypothesis of no autocorrelation. Must be one of `"two.sided"`,
#' `"less"`, or `"greater"`. Default is `"two.sided"`.
#'
#' @returns
#' A list with the following components:
#' * **observed** – The observed Moran's I.
#' * **expected** – The expected value of Moran's I under the null hypothesis.
#' * **sd** – The standard deviation of Moran's I under the null hypothesis.
#' * **p.value** – The p-value of the test based on the chosen `alternative`.
#'
#' @export
#'
#' @references
#' Gittleman, J. L., & Kot, M. (1990). Adaptation: statistics and a null model
#' for estimating phylogenetic effects. *Systematic Zoology*, 39(3), 227–241.
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Filter occurrences of Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Load example of raster variables
#' data("worldclim", package = "RuHere")
#' # Unwrap Packed raster
#' r <- terra::unwrap(worldclim)
#' # Extract values for bio_1
#' bio_1 <- terra::extract(r$bio_1,
#'                         occ[, c("decimalLongitude", "decimalLatitude")],
#'                         ID = FALSE, xy = TRUE)
#' #Remove NAs
#' bio_1 <- na.omit(bio_1)
#' # Convert values to numeric
#' v <- as.numeric(bio_1$bio_1)
#' # Compute geographic distance matrix
#' d <- fields::rdist.earth(x1 = as.matrix(bio_1[, c("x", "y")]), miles = FALSE)
#' # Inverse-distance weights
#' d <- 1/d
#' # Fill diagonal with 0
#' diag(d) <- 0
#' # Remove finite values
#' d[is.infinite(d)] <- 0
#' # Compute Moran's I
#' m <- moranfast(x = v, weight = d, scale = TRUE)
#' # Print results
#' m
moranfast <- function(x, weight, na_rm = TRUE, scaled = FALSE,
                      alternative = c("two.sided")) {
  # ---- Argument checking -----------------------------------------------------

  # x
  if (!inherits(x, "numeric")) {
    stop("`x` must be a numeric vector.", call. = FALSE)
  }
  if (!is.vector(x)) {
    stop("`x` must be a numeric *vector*, not a matrix or list.", call. = FALSE)
  }

  # weight
  if (!is.matrix(weight) || !is.numeric(weight)) {
    stop("`weight` must be a numeric matrix.", call. = FALSE)
  }
  if (nrow(weight) != length(x) || ncol(weight) != length(x)) {
    stop("`weight` must be a square matrix with dimensions equal to length of `x`.",
         call. = FALSE)
  }

  # na_rm
  if (!inherits(na_rm, "logical") || length(na_rm) != 1) {
    stop("`na_rm` must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # scaled
  if (!inherits(scaled, "logical") || length(scaled) != 1) {
    stop("`scaled` must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # alternative
  alt_allowed <- c("two.sided", "less", "greater")
  if (!inherits(alternative, "character") || length(alternative) != 1) {
    stop("`alternative` must be a single character string.", call. = FALSE)
  }
  if (!alternative %in% alt_allowed) {
    stop("`alternative` must be one of: 'two.sided', 'less', 'greater'.",
         call. = FALSE)
  }

  if (any(is.na(x)) && !na_rm) {
    stop("`x` contains NA values. Set `na_rm = TRUE` to remove them.",
         call. = FALSE)
  }

  if (any(is.na(x)) && na_rm) {
    keep <- which(!is.na(x))
    if (length(keep) < length(x)) {
      x <- x[keep]
      weight <- weight[keep, keep, drop = FALSE]
    }
  }

  # Additional consistency checks
  if (any(!is.finite(weight))) {
    warning("`weight` contains non-finite values. Consider replacing Inf with 0.",
            call. = FALSE)
  }

  # Call C++ function
  .Call(`_RuHere_moranfast`, x, weight, na_rm, scaled, alternative)
}

