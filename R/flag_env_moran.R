#' Select Environmentally Thinned Occurrences Using Moran's I Autocorrelation
#'
#' @description
#' This function evaluates multiple environmentally thinned datasets (produced
#' using different number of blocks) and selects the one that best balances
#' **low spatial autocorrelation** and **number of retained records**.
#'
#' For each number of bins provided in `n_bins`, the function computes Moran's I
#' for the selected environmental variables and summarizes autocorrelation using
#' a chosen statistic (mean, median, minimum, or maximum). The best thinning
#' level is then selected according to criteria described in *Details*.
#'
#' @param occ (data.frame or data.table) a data frame containing the occurrence
#' records for a **single species**. Must contain columns for species, longitude,
#' and latitude.
#' @param species (character) the name of the column in `occ` that contains the
#' species scientific names. Default is `"species"`.
#' @param long (character) the name of the column in `occ` that contains the
#' longitude values. Default is `"decimalLongitude"`.
#' @param lat (character) the name of the column in `occ` that contains the
#' latitude values. Default is `"decimalLatitude"`.
#' @param env_layers (SpatRaster) object containing environmental variables for
#' splitting in `n_bins` and for computing Moran's I.
#' @param n_bins (numeric) vector of number of bins into which each
#' environmental variable will be divided (e.g., c(5, 10, 15, 20)).
#' @param distance (character) distance metric used to compute the weight matrix
#' for Moran's I. One of `"haversine"` or `"euclidean"`. Default: `"haversine"`.
#' @param moran_summary (character) summary statistic used to select the best
#' thinning distance. One of `"mean"`, `"median"`, `"max"`, or `"min"`.
#' Default: `"mean"`.
#' @param min_records (numeric) minimum number of records required for a dataset
#' to be considered. Default: `10`.
#' @param min_imoran (numeric) minimum Moran's I required to avoid selecting
#' datasets with extremely low spatial autocorrelation. Default: `0.1`.
#' @param prioritary_column (character) name of a numeric columns in `occ`to
#' define retention priority (e.g., quality score, year). See details.
#' @param decreasing (logical) whether to sort records in decreasing order using
#' the `prioritary_column` (e.g., from most recent to oldest when the variable
#' is `"year"`). Only applicable when `prioritary_column` is not `NULL`.
#' Default is `TRUE`.
#' @param do_pca (logical) whether environmental variables should be summarized
#' using PCA before computing Moran's I. Default: `FALSE`. See details.
#' @param mask (SpatVector or SpatExtent) optional spatial object to mask the
#' `env_layers` before computing PCA. Only applicable if `do_pca = TRUE`.
#' Default is NULL.
#' @param pca_buffer (numeric) buffer width (km) used when PCA is computed from
#' the convex hull of records. Ignored if `mask` is provided. Default: `1000`.
#' @param flag_for_NA (logical) whether to treat records falling in `NA` cells
#' of `env_layers` as valid (`TRUE`) or invalid (`FALSE`). Default is `FALSE`.
#' @param return_all (logical) whether to return the full list of all thinned
#' datasets. Default is `FALSE`.
#' @param verbose (logical) whether to print messages about the progress.
#' Default is `TRUE`
#'
#' @details
#' This function is inspired by the approach used in Velazco et al. (2020),
#' extending the procedure by allowing:
#' - prioritization of records based on a user-defined variable (e.g., year)
#' - optional PCA transformation of environmental layers
#' - selection rules that prevent datasets with too few records or extremely
#'   low Moran's I from being chosen.
#'
#' **Procedure overview**
#' 1. For each bin number in `n_bins`, generate a spatially thinned dataset
#' using `thin_env()` function.
#' 2. Extract environmental values for the retained records.
#' 3. Compute Moran's I for each environmental variable.
#' 4. Summarize autocorrelation per dataset (mean, median, min, or max).
#' 5. Apply the selection criteria:
#'    - Keep only datasets with at least `min_records` records.
#'    - Keep only datasets with Moran's I ≥ `min_imoran`.
#'    - Round Moran's I to two decimal places and select the dataset with the
#'      **25th lowest** autocorrelation.
#'    - If more than on dataset is selected, choose the dataset retaining
#'    **more records**.
#'    - If still tied, choose the dataset with the **largest number of bins**.
#'
#' **Distance matrix for Moran's I**
#' Moran's I requires a weight matrix derived from pairwise distances among
#' records. Two distance types are available:
#' - `"haversine"`: geographic distance computed with `fields::rdist.earth()`
#'   (default; recommended for longitude/latitude coordinates)
#' - `"euclidean"`: Euclidean distance computed with `stats::dist()`
#'
#' **Environmental PCA (optional)**
#' If `do_pca = TRUE`, the environmental layers are summarized using PCA before
#' Moran's I is computed.
#' - If `mask` is provided, PCA is computed on masked layers.
#' - Otherwise, a convex hull around the records is buffered by `pca_buffer`
#'   kilometers to define the PCA area.
#' - It will select the axis that together explain more than 90% of the
#'   variation.
#'
#' @returns
#' A list with:
#' - **occ**: the selected thinned occurrence dataset with the column
#' `thin_env_flag`indicating whether each record is retained (`TRUE`) or flagged
#' as redundant (`FALSE`) in the environmental space .
#' - **imoran**: a table summarizing Moran's I for each thinning distance
#' - **n_bins**: the number of bins that produced the selected dataset
#' - **moran_summary**: the summary statistic used to select the dataset
#' - **all_thined**: (optional) list of thinned datasets for all bin numbers.
#' Only returned if `return_all` was set to `TRUE`
#'
#' @importFrom terra crop vect convHull buffer prcomp predict extract
#' @importFrom fields rdist.earth
#' @importFrom stats dist median quantile
#'
#' @export
#'
#' @examples
#' # Load example data
#' data("occurrences", package = "RuHere")
#' # Subset occurrences from Araucaria
#' occ <- occurrences[occurrences$species == "Araucaria angustifolia", ]
#' # Load example of raster variables
#' data("worldclim", package = "RuHere")
#' # Unwrap Packed raster
#' r <- terra::unwrap(worldclim)
#' # Select thinned occurrences
#' occ_env_moran <- flag_env_moran(occ = occ,
#'                                   n_bins = c(5, 10, 20, 30, 40, 50),
#'                                   env_layers = r)
#' # Selected number of bins
#' occ_env_moran$n_bins
#' # Number of flagged and unflagged records
#' sum(occ_env_moran$occ$thin_env_flag) #Retained
#' sum(!occ_env_moran$occ$thin_env_flag) #Flagged for thinning out
#' # Results os the spatial autocorrelation analysis
#' occ_env_moran$imoran
flag_env_moran <- function(occ,
                             species = "species",
                             long = "decimalLongitude",
                             lat = "decimalLatitude",
                             env_layers,
                             n_bins,
                             distance = "haversine",
                             moran_summary = "mean",
                             min_records = 10,
                             min_imoran = 0.1,
                             prioritary_column = NULL,
                             decreasing = TRUE,
                             do_pca = FALSE,
                             mask = NULL,
                             pca_buffer = 1000,
                             flag_for_NA = FALSE,
                             return_all = FALSE,
                             verbose = TRUE){

  # --- Argument checking -------------------------------------------------------

  # occ
  if (!inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table.", call. = FALSE)
  }

  # Force occ to be a dataframe
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)

  # species, long, lat must be single character values
  for (arg_name in c("species", "long", "lat")) {
    arg_value <- get(arg_name)
    if (!inherits(arg_value, "character") || length(arg_value) != 1) {
      stop("'", arg_name, "' must be a single character value.", call. = FALSE)
    }
  }

  # Check only one species
  sp <- unique(occ[[species]])
  n_spp <- length(sp)
  if (n_spp > 1) {
    stop("'occ' must contain occurrences of a single species")
  }

  # required columns in occ
  req_cols <- c(species, long, lat)
  missing_cols <- req_cols[!req_cols %in% names(occ)]
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing in 'occ': ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  # n_bins must be numeric vector with positive values
  if (!inherits(n_bins, "numeric")) {
    stop("'n_bins' must be a numeric vector.", call. = FALSE)
  }
  if (any(n_bins <= 0)) {
    stop("'n_bins' must contain only values > 0 (in km).", call. = FALSE)
  }

  # distance
  valid_dist <- c("haversine", "euclidean")
  if (!distance %in% valid_dist) {
    stop("'distance' must be one of: ",
         paste(valid_dist, collapse = ", "), ".", call. = FALSE)
  }

  # moran_summary
  valid_summ <- c("mean", "median", "max", "min")
  if (!moran_summary %in% valid_summ) {
    stop("'moran_summary' must be one of: ",
         paste(valid_summ, collapse = ", "), ".", call. = FALSE)
  }

  # min_records
  if (!inherits(min_records, "numeric") || length(min_records) != 1) {
    stop("'min_records' must be a single numeric value.", call. = FALSE)
  }
  if (min_records <= 0) {
    stop("'min_records' must be > 0.", call. = FALSE)
  }

  # min_imoran
  if (!inherits(min_imoran, "numeric") || length(min_imoran) != 1) {
    stop("'min_imoran' must be a single numeric value.", call. = FALSE)
  }

  # prioritary_column
  if (!is.null(prioritary_column)) {

    if (!inherits(prioritary_column, "character") || length(prioritary_column) != 1) {
      stop("'prioritary_column' must be a single character value.", call. = FALSE)
    }

    if (!prioritary_column %in% names(occ)) {
      stop("Column '", prioritary_column, "' not found in 'occ'.", call. = FALSE)
    }

    if (!inherits(occ[[prioritary_column]], "numeric")) {
      stop("'prioritary_column' must refer to a numeric column.", call. = FALSE)
    }
  }

  # decreasing
  if (!inherits(decreasing, "logical") || length(decreasing) != 1) {
    stop("'decreasing' must be TRUE or FALSE.", call. = FALSE)
  }

  # env_layers
  if (!inherits(env_layers, "SpatRaster")) {
    stop("'env_layers' must be a SpatRaster object.", call. = FALSE)
  }

  # do_pca
  if (!inherits(do_pca, "logical") || length(do_pca) != 1) {
    stop("'do_pca' must be TRUE or FALSE.", call. = FALSE)
  }

  # mask
  if (!is.null(mask) &&
      !inherits(mask, c("SpatVector", "SpatExtent"))) {
    stop("'mask' must be a SpatVector, SpatExtent, or NULL.", call. = FALSE)
  }

  # pca_buffer
  if (!inherits(pca_buffer, "numeric") || length(pca_buffer) != 1) {
    stop("'pca_buffer' must be a single numeric value.", call. = FALSE)
  }
  if (pca_buffer <= 0) {
    stop("'pca_buffer' must be > 0.", call. = FALSE)
  }

  # return_all
  if (!inherits(return_all, "logical") || length(return_all) != 1) {
    stop("'return_all' must be TRUE or FALSE.", call. = FALSE)
  }

  #--- flag_for_NA must be logical ---
  if (!is.logical(flag_for_NA) || length(flag_for_NA) != 1) {
    stop("'flag_for_NA' must be TRUE or FALSE")
  }

  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)

  spp <- unique(occ[[species]])


  if(do_pca){
    if(!is.null(mask)){
      env_layers <- terra::crop(env_layers, mask, mask = TRUE)
    } else {
      pts <- terra::vect(occ, geom = c(x = long, y = lat),
                         crs = terra::crs(env_layers))
      ca <- terra::convHull(pts)
      ca <- terra::buffer(ca, width = pca_buffer *1000)
      env_layers <- terra::crop(env_layers, ca, mask = TRUE)
    }

    #Run PCA of variables in the pseudo_M
    pca <- terra::prcomp(env_layers, scale = TRUE, center = TRUE)

    #Select axis that explain 90%
    variance_explained <- cumsum(pca[["sdev"]] / sum(pca[["sdev"]]) * 100)
    n_axis <- length(variance_explained) - (length(variance_explained[variance_explained > 90]) - 1)
    #Predict variables to space
    env_layers <- terra::predict(env_layers, pca, index = 1:n_axis)
  } #End of PCA

  #Filter using distances
  if(verbose) message("Filtering records...")
  filtered <- suppressMessages(lapply(n_bins, function(x){
    set.seed(42)
    f_x <- thin_env(occ = occ, species = species,
                    long = long, lat = lat,
                    n_bins = x,
                    env_layers = env_layers,
                    prioritary_column = prioritary_column,
                    decreasing = decreasing,
                    flag_for_NA = flag_for_NA)
    return(f_x$thin_env_flag)
  }))

  #Rename list with distances
  names(filtered) <- n_bins

  ## Calculate spatial autoccorelation (Moran I)
  if(verbose) message("Calculating spatial autocorrelation using Moran Index...")
  imoran <- lapply(names(filtered), function(x){
    tryCatch({ #Avoid errors
      # Append results to occ
      filtered_x <- cbind(occ[, c(long, lat)], "thin_env_flag" = filtered[[x]])
      # Remove flagged
      filtered_x <- filtered_x[filtered_x$thin_env_flag, ]
      coord <- filtered_x[, c(long, lat)]
      # Extract data
      data <- data.frame(terra::extract(env_layers, coord, ID = FALSE))
      # Get matrix of inverse distance
      if(distance == "haversine"){
        distm <- fields::rdist.earth(x1 = coord, miles = FALSE)
      } else if(distance == "euclidean"){
        distm <- as.matrix(stats::dist(coord))
      }
      # Get inverse of distance
      distm <-  1/distm
      # Remove non finite values
      diag(distm) <- 0
      distm[is.infinite(distm)] <- 0
      imoran_x <- apply(data, 2, function(x){
        moranfast(x = as.numeric(x), weight = distm, na_rm = TRUE,
                  scaled = TRUE)$observed})
    })
  })
  names(imoran) <- n_bins
  imorandf <- as.data.frame(do.call("rbind", imoran))

  #Get distances available
  d_i <- as.character(n_bins[sapply(imoran, function(i) inherits(i, "numeric"))])
  imorandf$n_bins <- d_i
  imorandf <- relocate_before(imorandf, "n_bins", names(imorandf)[1])


  #Get mean, median, min and max of imoran across PCA variables
  cols <- names(env_layers)
  m <- imorandf[, cols, drop = FALSE]
  imorandf$median_moran <- apply(m, 1, stats::median)
  imorandf$mean_moran <- rowMeans(m)
  imorandf$min_moran <- apply(m, 1, min)
  imorandf$max_moran <- apply(m, 1, max)


  #Get number of records remained in each distance
  imorandf$n_filtered <- sapply(filtered[d_i], sum, na.rm = TRUE)

  #Put name of the specie in the dataframe and total number of records
  imorandf$species <- spp
  imorandf <- relocate_before(imorandf, "species", names(imorandf)[1])
  imorandf$all_records <- nrow(occ)

  #Propotion of lost records
  imorandf$prop_lost <- (imorandf$all_records -
                           imorandf$n_filtered)/imorandf$all_records

  # Get moran summary to filter
  moran_summary <- paste0(moran_summary, "_moran")

  #Filtering distances: select lower autocorrelation (first quantile) which
  # keeps the maximum number of occurrences
  if(min(imorandf[[moran_summary]]) > 0) {
    # 1. Create column `to_filter` based on the value in `moran_summary`
    finalfilter <- imorandf
    finalfilter$to_filter <- finalfilter[[moran_summary]]
    # 2. Filter: keep rows where to_filter > min_imoran
    finalfilter <- finalfilter[finalfilter$to_filter > min_imoran, ,
                               drop = FALSE]
    # 3. Filter: keep rows where n_filtered >= min_records
    finalfilter <- finalfilter[finalfilter$n_filtered >= min_records, ,
                               drop = FALSE]
    # 4. Round `to_filter` to 2 decimal places
    finalfilter$to_filter <- round(finalfilter$to_filter, 2)
    # 5. Filter: keep rows where to_filter <= 25th percentile
    q25 <- stats::quantile(finalfilter$to_filter)[2]
    finalfilter <- finalfilter[finalfilter$to_filter <= q25, , drop = FALSE]
    # 6. Filter: keep only rows with the maximum n_filtered
    max_n <- max(finalfilter$n_filtered)
    finalfilter <- finalfilter[finalfilter$n_filtered == max_n, , drop = FALSE]
    # 7. Keep only the row with the maximum n_bins
    max_bins <- max(finalfilter$n_bins)
    finalfilter <- finalfilter[finalfilter$n_bins == max_bins, , drop = FALSE]
    # 8. Remove temporary column
    finalfilter$to_filter <- NULL
  } else {
    finalfilter <- imorandf[imorandf$n_bins == max(imorandf$n_bins), ,
                            drop = FALSE]
  }

  #Get final points
  final_points <- filtered[[as.character(finalfilter$n_bins)]]
  occ_final <- cbind(occ, "thin_env_flag" = final_points)

  #Return final points and imoran table
  if(!return_all){
    return(list(occ = occ_final,
                imoran = imorandf,
                n_bins = finalfilter$n_bins,
                moran_summary = moran_summary))
  } else {
    all_thinned <- lapply(names(filtered), function(x){
      cbind(occ, "thin_env_flag" = filtered[[x]])
    })
    names(all_thinned) <- names(filtered)
    return(list(occ = occ_final,
                imoran = imorandf,
                n_bins = finalfilter$n_bins,
                moran_summary = moran_summary,
                all_thinned = all_thinned))
  }

  }
