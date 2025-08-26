prepare_wcvp_data <- function(wcp_names,
                              wcp_dist,
                              wcp_map){
  # wcp_names <- rWCVPdata::wcvp_names
  # wcp_dist <- rWCVPdata::wcvp_distributions
  # wcp_map <- rWCVPdata::wgsrpd3

  #Get distributions
  wcp_names <- wcp_names %>% dplyr::select(plant_name_id, species = taxon_name)
  wcp_dist <- wcp_dist %>% dplyr::filter(introduced == 0, extinct == 0, #Only native
                                    location_doubtful == 0) %>%
    dplyr::select(plant_name_id, LEVEL3_COD = area_code_l3, area_code_l3)
  wcp <- left_join(wcp_names, wcp_dist, by = "plant_name_id") %>% na.omit() %>%
    dplyr::select(-plant_name_id)

  #Select columns in wcp_map
  wcp_map <- vect(wcp_map[,c("LEVEL3_NAM", "LEVEL3_COD")])
  #plot(wcp_map)

  return(list(wcp_data = wcp,
              wcp_map = wcp_map))
}

filter_wcvp <- function(occ, species, x, y, wcvp_data, wcvp_map, buffer = 50,
                        return_map = TRUE){
  #Get species in
  all_sp <- unique(occ[[species]])
  sp_in <- intersect(all_sp, wcvp_data$species)
  if(length(sp_in) == 0){
    stop("None of the species in occ are within wcvp_data")
  }
  sp_out <- setdiff(all_sp, wcvp_data$species)
  if(length(sp_out) > 0){
    warning("Some of the species in occ are not present in wcvp_data")
    occ_out <- lapply(sp_out, function(i){
      occ_out <- occ[occ[[species]] == i, ]
      occ_out$natural_range_wcvp <- NA
      return(list(flagged = occ_out))
    })
    names(occ_out) <- sp_out

  }

  #Filter species
  occ_filt <- pbapply::pblapply(sp_in, function(i){
      sp_dist <- wcvp_data %>% filter(species == i) %>% pull(LEVEL3_COD)
      m_sp <- wcvp_map[wcvp_map$LEVEL3_COD %in% sp_dist,]
      #Buffer of x km
      m_sp <- terra::aggregate(m_sp) %>% terra::buffer(., width = buffer*1000)
      #Get occurrences and convert to pts
      occ_sp <- occ[occ[[species]] == i, ]
      pts_sp <- occ_sp %>% terra::vect(geom = c(x = "decimalLongitude",
                                                y ="decimalLatitude"),
                                       crs = "+init=epsg:4326")
      #Occurrences inside natural range
      pts_in <- is.related(pts_sp, m_sp, "intersects")
      #Create column to save results
      occ_sp$natural_range_wcvp <- pts_in

      # #Plot to check
      # pts2 <- occ_sp %>% terra::vect(geom = c(x = "decimalLongitude",
      #                                        y ="decimalLatitude"))
      # mapview(m_sp) + mapview(pts2, zcol = "nat_range_wcvp")

      if(!return_map){
        m_sp <- NULL
      }
      return(list(flagged = occ_sp,
                  map = m_sp))
  })
  names(occ_filt) <- sp_in

  if(length(sp_out) > 0){
    occ_filt <- c(occ_filt, occ_out)
  }
  return(occ_filt)
}
