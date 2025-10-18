bind_here <- function(...,
                      df_class = "data.frame",
                      fill = FALSE){
  l <- list(...)
  if(df_class == "data.frame") {
    return(as.data.frame(data.table::rbindlist(l)))
    } else if (df_class == "data.table"){
      return(data.table::rbindlist(l))
    }
}

# all_occ <- bind_here(occ_splink, occ_gbif, occ_bien)
