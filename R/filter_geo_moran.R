filter_geo_moran <- function(occ, species, long, lat, d, variables){
  sp <- occ[1, species]

  occ <- data.frame(occ)

  #Create pseudo-m: a minimum convex polygon with a buffer of 1000km
  #We set this initial buffer to decrease the time of filtering
  pts <- vect(occ, geom = c(x = long, y = lat),
              crs = crs(variables))
  ca <- terra::convHull(pts)
  ca <- buffer(ca, width = 1000*1000)

  #Cut variables
  var_ca <- crop(variables, ca, mask = TRUE)

  #Run PCA of variables in the pseudo_M
  pca <- prcomp(var_ca, scale = TRUE, center = TRUE)

  #Select axis that explain 90%
  variance_explained <- cumsum(pca[["sdev"]] / sum(pca[["sdev"]]) * 100)
  n_axis <- length(variance_explained) - (length(variance_explained[variance_explained > 90]) - 1)
  #Predict variables to space
  var_sp <- terra::predict(var_ca, pca, index = 1:n_axis)


  #Filter using distances
  message("Filtering records...")
  filtered <- suppressMessages(pbapply::pblapply(d, function(x){
    set.seed(42)
    thin(loc.data = occ,
         lat.col = lat, long.col = long,
         spec.col = species,
         thin.par = x, reps = 1, locs.thinned.list.return = TRUE,
         write.files = FALSE, write.log.file = FALSE, verbose = FALSE)[[1]]
  }))


  #Rename list with distances
  names(filtered) <- d

  ## Calculate spatial autoccorelation (Moran I)
  message("Calculating spatial autocorrelation using Moran Index...")
  imoran <- lapply(names(filtered), function(x){
    tryCatch({ #Avoid errors
      coord <- filtered[[x]] %>% dplyr::select(Longitude, Latitude)
      data <- data.frame(terra::extract(var_sp, coord, ID = FALSE))
      coord <- coord[!is.na(data$PC1),]
      data <- data[!is.na(data$PC1),]
      imoran_x <- apply(data, 2, function(x)
        moranfast(x, coord$Longitude, coord$Latitude)$observed)
      imoran_x <- c("Distance" = as.numeric(x), imoran_x)
    },
    error=function(e) NULL) #Avoid errors
  })
  names(imoran) <- d
  imorandf <- do.call("rbind", imoran) %>% as.data.frame()

  #Get distances available
  d_i <- as.character(imorandf$Distance)

  #Get mean of imoran across PCA variables
  imorandf <- imorandf %>% dplyr::mutate(
    median_moran=apply(imorandf[, names(var_sp)], 1, median))

  #Get number of records remained in each distance
  imorandf$n_filtered <- sapply(filtered[d_i], nrow)

  #Put name of the specie in the dataframe and total number of records
  imorandf <- imorandf %>% mutate(species = sp, .before = 1) %>%
    mutate(all_records = nrow(occ))
  #Propotion of lost records
  imorandf$prop_lost <- (imorandf$all_records - imorandf$n_filtered)/imorandf$all_records

  #Filtering distances: select lower autocorrelation (first quantile) which keeps the maximum number of occurrences
  if(min(imorandf$median_moran) > 0) {
    finalfilter <- imorandf %>%
      filter(median_moran > 0) %>% #Only positive imorans
      filter(n_filtered >= 10) %>% #Only filters with 10 or more records
      mutate(median_moran = round(median_moran, 2)) %>% #Keep only 2 decimal places
      dplyr::filter(median_moran<=quantile(median_moran)[2]) %>% # Select 25th lower spatial autocorrelation
      dplyr::filter(n_filtered==max(n_filtered)) %>% # Select distance with higher number of records
      dplyr::sample_n(1) # Select a random value if more than one is selected
  } else {
    finalfilter <- imorandf %>% filter(Distance == min(Distance))
  }
  #Get final points
  final_points <- cbind("species" = sp,
                        filtered[[finalfilter$Distance %>% as.character()]])
  colnames(final_points)[2:3] <- c("x", "y")

  #Return final points and imoran table
  return(list(occ = final_points,
              imoran = imorandf,
              Distance = finalfilter$Distance))
  }

# #occ2 <- occ
# occ <- occ2
# long = "decimalLongitude"
# lat = "decimalLatitude"
# species = "scientificName"
# variables = v[[-22]]
# d = c(0, 5, 7, 10, 12, 15, 20, 25, 30)

filter_geo_moran_ape <- function(occ, species, long, lat, d, variables){
  sp <- occ[1, species]

  occ <- data.frame(occ)

  #Create pseudo-m: a minimum convex polygon with a buffer of 1000km
  #We set this initial buffer to decrease the time of filtering
  pts <- vect(occ, geom = c(x = long, y = lat),
              crs = crs(variables))
  ca <- terra::convHull(pts)
  ca <- buffer(ca, width = 1000*1000)

  #Cut variables
  var_ca <- crop(variables, ca, mask = TRUE)

  #Run PCA of variables in the pseudo_M
  pca <- prcomp(var_ca, scale = TRUE, center = TRUE)

  #Select axis that explain 90%
  variance_explained <- cumsum(pca[["sdev"]] / sum(pca[["sdev"]]) * 100)
  n_axis <- length(variance_explained) - (length(variance_explained[variance_explained > 90]) - 1)
  #Predict variables to space
  var_sp <- terra::predict(var_ca, pca, index = 1:n_axis)


  #Filter using distances
  message("Filtering records...")
  filtered <- suppressMessages(pbapply::pblapply(d, function(x){
    set.seed(42)
    thin(loc.data = occ,
         lat.col = lat, long.col = long,
         spec.col = species,
         thin.par = x, reps = 1, locs.thinned.list.return = TRUE,
         write.files = FALSE, write.log.file = FALSE, verbose = FALSE)[[1]]
  }))


  #Rename list with distances
  names(filtered) <- d

  ## Calculate spatial autoccorelation (Moran I)
  message("Calculating spatial autocorrelation using Moran Index...")
  imoran <- lapply(names(filtered), function(x){
    tryCatch({ #Avoid errors
      coord <- filtered[[x]] %>% dplyr::select(Longitude, Latitude)
      distm <- as.matrix(coord) %>% dist() %>% as.matrix()
      distm <-  1/distm
      diag(distm) <- 0
      distm[is.infinite(distm)] <- 0 #Remover valores finitos
      data <- data.frame(terra::extract(var_sp, coord, ID = FALSE))
      # Calculate Moran
      imoran_x <- apply(data, 2, function(x)
        ape::Moran.I(x, distm, scaled = T, na.rm = TRUE)$observed)
      imoran_x <- c("Distance" = as.numeric(x), imoran_x)
    },
    error=function(e) NULL) #Avoid errors
  })
  names(imoran) <- d
  imorandf <- do.call("rbind", imoran) %>% as.data.frame()

  #Get distances available
  d_i <- as.character(imorandf$Distance)

  #Get mean of imoran across PCA variables
  imorandf <- imorandf %>% dplyr::mutate(
    median_moran=apply(imorandf[, names(var_sp)], 1, median))

  #Get number of records remained in each distance
  imorandf$n_filtered <- sapply(filtered[d_i], nrow)

  #Put name of the specie in the dataframe and total number of records
  imorandf <- imorandf %>% mutate(species = sp, .before = 1) %>%
    mutate(all_records = nrow(occ))
  #Propotion of lost records
  imorandf$prop_lost <- (imorandf$all_records - imorandf$n_filtered)/imorandf$all_records

  #Filtering distances: select lower autocorrelation (first quantile) which keeps the maximum number of occurrences
  if(min(imorandf$median_moran) > 0) {
    finalfilter <- imorandf %>%
      filter(median_moran > 0) %>% #Only positive imorans
      filter(n_filtered >= 10) %>% #Only filters with 10 or more records
      mutate(median_moran = round(median_moran, 2)) %>% #Keep only 2 decimal places
      dplyr::filter(median_moran<=quantile(median_moran)[2]) %>% # Select 25th lower spatial autocorrelation
      dplyr::filter(n_filtered==max(n_filtered)) %>% # Select distance with higher number of records
      dplyr::sample_n(1) # Select a random value if more than one is selected
  } else {
    finalfilter <- imorandf %>% filter(Distance == min(Distance))
  }
  #Get final points
  final_points <- cbind("species" = sp,
                        filtered[[finalfilter$Distance %>% as.character()]])
  colnames(final_points)[2:3] <- c("x", "y")

  #Return final points and imoran table
  return(list(occ = final_points,
              imoran = imorandf,
              Distance = finalfilter$Distance))
}

# #occ2 <- occ
# occ <- occ2
# long = "decimalLongitude"
# lat = "decimalLatitude"
# species = "scientificName"
# variables = v[[-22]]
# d = c(0, 5, 7, 10, 12, 15, 20, 25, 30)

