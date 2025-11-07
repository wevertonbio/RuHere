#' Prepare data to request GBIF download
#'
#' @usage prepare_gbif_download(species, rank = NULL, kingdom = NULL,
#'                              phylum = NULL, class = NULL, order = NULL,
#'                              family = NULL, genus = NULL, strict = FALSE,
#'                              ...)
#'
#' @param species (character) a vector of species name(s).
#' @param rank (character) optional taxonomic rank (for example, 'species' or
#' 'genus'). Default is NULL, meaning it will return species matched across all
#' ranks.
#' @param kingdom (character) optional taxonomic kingdom (for example, 'Plantae'
#' or 'Animalia'). Default is NULL, meaning it will return species matched
#' across all kingdoms.
#' @param phylum (character) optional taxonomic phylum. Default is NULL, meaning
#' it will return species matched across all phyla.
#' @param class (character) optional taxonomic class. Defaults is NULL, meaning
#' it will return species matched across all classes.
#' @param order (character) optional taxonomic order. Defaults is NULL, meaning
#' it will return species matched across all orders
#' @param family (character) optional taxonomic family. Defaults is NULL,
#' meaning it will return species matched across all families.
#' @param genus (character) optional taxonomic genus. Defaults is NULL, meaning
#' it will return species matched across all genus.
#' @param strict (logical) If TRUE, it (fuzzy) matches only the given name, but
#' never a taxon in the upper classification. Default is FALSE.
#' @param ... other parameters passed to `rgbif::occ_count()`.
#'
#' @note
#' This function requires an active internet connection to access GBIF data.
#'
#' @returns
#' A data.frame with species information, including the number of occurrences
#' and other related details.
#'
#' @importFrom pbapply pblapply
#' @importFrom rgbif name_backbone occ_count
#' @importFrom dplyr mutate %>% relocate
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
#' }
prepare_gbif_download <- function(species, rank = NULL, kingdom = NULL,
                                  phylum = NULL, class = NULL, order = NULL,
                                  family = NULL, genus = NULL, strict = FALSE,
                                  ...){
  # Check arguments
  if (!inherits(species, "character"))
    stop("'species' must be a character vector, not ", class(species),
         call. = FALSE)

  if (!is.null(rank) && !inherits(rank, "character"))
    stop("'rank' must be a character value or NULL, not ", class(rank),
         call. = FALSE)

  if (!is.null(rank) && length(rank) != 1)
    stop("'rank' must be a single character value.", call. = FALSE)

  if (!is.null(kingdom) && !inherits(kingdom, "character"))
    stop("'kingdom' must be a character value or NULL, not ", class(kingdom),
         call. = FALSE)

  if (!is.null(phylum) && !inherits(phylum, "character"))
    stop("'phylum' must be a character value or NULL, not ", class(phylum),
         call. = FALSE)

  if (!is.null(class) && !inherits(class, "character"))
    stop("'class' must be a character value or NULL, not ", class(class),
         call. = FALSE)

  if (!is.null(order) && !inherits(order, "character"))
    stop("'order' must be a character value or NULL, not ", class(order),
         call. = FALSE)

  if (!is.null(family) && !inherits(family, "character"))
    stop("'family' must be a character value or NULL, not ", class(family),
         call. = FALSE)

  if (!is.null(genus) && !inherits(genus, "character"))
    stop("'genus' must be a character value or NULL, not ", class(genus),
         call. = FALSE)

  if (!inherits(strict, "logical") || length(strict) != 1)
    stop("'strict' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)


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

  res <- as.data.frame(data.table::rbindlist(res))
  class(res) <- c("data.frame", "gbif_info")
  return(res)
}
