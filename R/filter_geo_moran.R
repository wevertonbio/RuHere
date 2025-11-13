#' Title
#'
#' @param occ
#' @param species
#' @param long
#' @param lat
#' @param d
#' @param moran_summary
#' @param prioritary_column
#' @param desc
#' @param raster_variables
#' @param do_pca
#' @param pca_buffer
#' @param mask
#'
#' @returns
#' @export
#'
#' @examples
filter_geo_moran <- function(occ,
                             species = "species",
                             long = "decimalLongitude",
                             lat = "decimalLatitude",
                             d,
                             moran_summary = "mean",
                             prioritary_column = NULL,
                             desc = TRUE,
                             raster_variables,
                             do_pca = FALSE,
                             pca_buffer = 1000,
                             mask = NULL){

  sp <- unique(occ[[species]])

  occ <- data.frame(occ)


  if(do_pca){
    if(!is.null(mask)){
      raster_variables <- terra::crop(raster_variables, mask, mask = TRUE)
    } else {
      pts <- terra::vect(occ, geom = c(x = long, y = lat),
                       crs = crs(raster_variables))
      ca <- terra::convHull(pts) %>%
        terra::buffer(ca, width = pca_buffer *1000)
      raster_variables <- terra::crop(raster_variables, ca, mask = TRUE)
    }

    #Run PCA of variables in the pseudo_M
    pca <- terra::prcomp(raster_variables, scale = TRUE, center = TRUE)

    #Select axis that explain 90%
    variance_explained <- cumsum(pca[["sdev"]] / sum(pca[["sdev"]]) * 100)
    n_axis <- length(variance_explained) - (length(variance_explained[variance_explained > 90]) - 1)
    #Predict variables to space
    raster_variables <- terra::predict(raster_variables, pca, index = 1:n_axis)
  } #End of PCA

  #Filter using distances
  message("Filtering records...")
  filtered <- suppressMessages(pbapply::pblapply(d, function(x){
    set.seed(42)
    f_x <- thin_records(occ = occ,
                 long = lat, lat = long,
                 d = x,
                 species = species,
                 prioritary_column = prioritary_column, desc = desc)
    return(f_x[f_x$thin_flag,])
  }))


  #Rename list with distances
  names(filtered) <- d

  ## Calculate spatial autoccorelation (Moran I)
  message("Calculating spatial autocorrelation using Moran Index...")
  imoran <- lapply(names(filtered), function(x){
    tryCatch({ #Avoid errors
      coord <- filtered[[x]][, c(long, lat)]
      # Extract data
      data <- data.frame(terra::extract(raster_variables, coord, ID = FALSE))
      # Get matrix of inverse distance
      distm <- as.matrix(coord) %>% dist() %>% as.matrix()
      distm <-  1/distm
      diag(distm) <- 0
      distm[is.infinite(distm)] <- 0 #Remover valores finitos
      imoran_x <- apply(data, 2, function(x){
        moranfast(x = as.numeric(x), weight = distm, na_rm = TRUE,
                  scaled = TRUE)$observed})
  })
  })
  names(imoran) <- d
  imorandf <- do.call("rbind", imoran) %>% as.data.frame()

  #Get distances available
  d_i <- d[sapply(imoran, function(i) inherits(i, "numeric"))] %>%
    as.character()
  imorandf <- imorandf %>% dplyr::mutate(Distance = d_i, .before = 1)

  #Get mean, median, min and max of imoran across PCA variables
  imorandf <- imorandf %>% dplyr::mutate(
    median_moran=apply(imorandf[, names(raster_variables)], 1, median),
    mean_moran=apply(imorandf[, names(raster_variables)], 1, mean),
    min_moran=apply(imorandf[, names(raster_variables)], 1, min),
    max_moran=apply(imorandf[, names(raster_variables)], 1, max))

  #Get number of records remained in each distance
  imorandf$n_filtered <- sapply(filtered[d_i], nrow)

  #Put name of the specie in the dataframe and total number of records
  imorandf <- imorandf %>% dplyr::mutate(species = sp, .before = 1) %>%
    dplyr::mutate(all_records = nrow(occ))

  #Propotion of lost records
  imorandf$prop_lost <- (imorandf$all_records - imorandf$n_filtered)/imorandf$all_records

  # Get moran summary to filter
  moran_summary <- paste0(moran_summary, "_moran")

  #Filtering distances: select lower autocorrelation (first quantile) which keeps the maximum number of occurrences
  if(min(imorandf[[moran_summary]]) > 0) {
    finalfilter <- imorandf %>%
      mutate(to_filter = !!dplyr::sym(moran_summary)) %>%
      filter(to_filter > 0) %>% #Only positive imorans
      filter(n_filtered >= 10) %>% #Only filters with 10 or more records
      mutate(to_filter = round(to_filter, 2)) %>% #Keep only 2 decimal places
      dplyr::filter(to_filter<=quantile(to_filter)[2]) %>% # Select 25th lower spatial autocorrelation
      dplyr::filter(n_filtered==max(n_filtered)) %>% # Select distance with higher number of records
      dplyr::slice_min(Distance, n = 1) %>% # Select lowest if more than one is selected
      dplyr::select(-to_filter)
  } else {
    finalfilter <- imorandf %>% filter(Distance == min(Distance))
  }
  #Get final points
  final_points <- filtered[[finalfilter$Distance %>% as.character()]]

  #Return final points and imoran table
  return(list(occ = final_points,
              imoran = imorandf,
              Distance = finalfilter$Distance,
              moran_summary = moran_summary))
}

# #occ2 <- occ
# occ <- occ2
# long = "decimalLongitude"
# lat = "decimalLatitude"
# species = "scientificName"
# variables = v[[-22]]
# d = c(0, 5, 7, 10, 12, 15, 20, 25, 30)

