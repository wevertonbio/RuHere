prepare_gbif_download <- function(species, rank = NULL, kingdom = NULL,
                                  phylum = NULL, class = NULL, order = NULL,
                                  family = NULL, genus = NULL, strict = FALSE,
                                  ...){ #Add ... in functions with occ_search

  res <- pbapply::pblapply(species, function(sp){
    # Get taxonomy info
    gbif_info <- rgbif::name_backbone(name = sp, rank, kingdom,
                                      phylum, class, order,
                                      family, genus, strict)

    # Get number of records (all)
    n <- rgbif::occ_count(taxonKey = gbif_info$usageKey)
    # Number of records (only with coordinates)
    n_with_xy <- rgbif::occ_count(taxonKey = gbif_info$usageKey,
                                  hasCoordinate = TRUE, ...)
    # Append information
    gbif_info <- gbif_info %>% dplyr::mutate(n_records = n,
                                             with_coordinates = n_with_xy,
                                             .before = canonicalName) %>%
      dplyr::relocate(species)

  })

  return(as.data.frame(rbindlist(res)))
}


# # Exemplo
# species = "Araucaria angustifolia"
# rank = NULL
# kingdom = NULL
# phylum = NULL
# class = NULL
# order = NULL
# family = NULL
# genus = NULL
# strict = FALSE

# # Teste
# gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
# Save this as data
# usethis::use_data(gbif_prepared)

