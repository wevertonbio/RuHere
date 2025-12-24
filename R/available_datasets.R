#' Check the available distribution datasets for a set of species
#'
#' @description
#' This function checks which datasets contain distributional information for a
#' given set of species, based on expert-curated sources. It searches the
#' selected datasets and reports whether each species has available distribution
#' data.
#'
#' @param data_dir (character) directory path where the datasets were saved.
#' See *Details* for more information.
#' @param species (character) vector with the species names to be checked for
#' the availability of distributional information.
#' @param datasets (character) vector indicating which datasets to search.
#' Options are `"all"`, `"florabr"`, `"wcvp"`, `"iucn"`, `"bien"`, and
#' `"faunabr"`. Default searches all datasets.
#' @param return_distribution (logical) whether to return the spatial objects
#' (`SpatVector`) representing the distribution regions of the species found in
#' the selected datasets. Default is `FALSE`.
#'
#' @details
#' The distribution datasets can be obtained using the functions
#' `florabr_here()`, `wcvp_here()`, `bien_here()`, and `faunabr_here()`,
#' which download and prepare the corresponding sources for use in `RuHere`.
#'
#'
#' @returns
#' If `return_distribution = FALSE`, a data.frame containing the species names
#' and the datasets where distributional information is available.
#' If `return_distribution = TRUE`, it also returns a list containing the
#' `SpatVector` objects representing the species ranges.
#'
#' @export
#'
#' @importFrom florabr load_florabr get_spat_occ
#' @importFrom faunabr load_faunabr fauna_spat_occ
#' @importFrom data.table fread rbindlist
#' @importFrom terra vect as.data.frame aggregate
#'
#' @examples
#' # Set directory where datasets were saved
#' # Here, we'll use the directory where the example datasets are stored
#' datadir <- system.file("extdata", "datasets",  package = "RuHere")
#' # Check available datasets
#' d <- available_datasets(data_dir = datadir,
#'                         species = c("Araucaria angustifolia",
#'                                     "Handroanthus serratifolius",
#'                                     "Cyanocorax caeruleus"))
#' # Check available datasets and return distribution
#' d2 <- available_datasets(data_dir = datadir,
#'                          species = c("Araucaria angustifolia",
#'                                      "Handroanthus serratifolius",
#'                                      "Cyanocorax caeruleus"),
#'                          return_distribution = TRUE)
#'
available_datasets <- function(data_dir, species, datasets = "all",
                               return_distribution = FALSE){

  # ---- Argument checking ----

  # data_dir
  if (missing(data_dir) || is.null(data_dir)) {
    stop("'data_dir' must be provided (must not be NULL or missing).")
  } else if (!inherits(data_dir, "character")) {
    stop("'data_dir' must be a character, not ", class(data_dir))
  }
  if (!dir.exists(data_dir)) {
    stop("Directory specified in 'data_dir' does not exist: ", data_dir)
  }

  # species
  if (missing(species) || is.null(species)) {
    stop("'species' must be provided (must not be NULL or missing).")
  } else if (!inherits(species, "character")) {
    stop("'species' must be a character vector, not ", class(species))
  }

  # datasets
  valid_datasets <- c("all", "florabr", "wcvp", "iucn", "bien", "faunabr")

  if (!inherits(datasets, "character")) {
    stop("'datasets' must be a character vector, not ", class(datasets))
  }

  # if not "all", check if datasets are valid
  if (!("all" %in% datasets)) {
    invalid <- setdiff(datasets, valid_datasets)
    if (length(invalid) > 0) {
      stop("Invalid dataset name(s): ", paste(invalid, collapse = ", "),
           ". Valid options are: ", paste(valid_datasets, collapse = ", "))
    }
  }

  # return_distribution
  if (!inherits(return_distribution, "logical")) {
    stop("'return_distribution' must be logical, not ", class(return_distribution))
  }


  # Check datasets available
  if(datasets == "all"){
    l <- list.dirs(data_dir, recursive = FALSE, full.names = FALSE)
    l <- intersect(l,
                   c("florabr", "wcvp", "iucn", "bien", "faunabr"))
  } else {
    l <- datasets
  }

  # Import florabr
  if("florabr" %in% l){
    flora <- florabr::load_florabr(data_dir = file.path(data_dir, "florabr"),
                                   verbose = FALSE)}

  if("faunabr" %in% l){
    fauna <- faunabr::load_faunabr(data_dir = file.path(data_dir, "faunabr"),
                                   verbose = FALSE)}

  if("wcvp" %in% l){
    wcvp <- data.table::fread(file.path(data_dir = file.path(
      data_dir, "wcvp", "wcvp.gz")))
  }

  if("iucn" %in% l){
    iucn <- data.table::fread(file.path(data_dir = file.path(
      data_dir, "iucn", "iucn_distribution.gz")))
  }

  # Get map
  if("iucn" %in% l | "wcvp" %in% l){
    if("iucn" %in% l & "wcvp" %in% l)
      m <- terra::vect(file.path(data_dir, "wgsrpd/wgsrpd.gpkg"))

    if("iucn" %in% l & !("wcvp" %in% l))
      m <- terra::vect(file.path(data_dir, "wgsrpd/wgsrpd.gpkg"))

    if("wcvp" %in% l & !("iucn" %in% l))
      m <- terra::vect(file.path(data_dir, "wgsrpd/wgsrpd.gpkg"))

    # Get dataframe to filter levels
    dm <- terra::as.data.frame(m)
  }

  r_species <- lapply(species, function(x){
    # Check if species x is in dataset
    r <- lapply(l, function(i){

      if(i == "bien"){
        sp_bien <- gsub(" ", "_", x)
        return(file.exists(file.path(data_dir, paste0("bien/", sp_bien, ".gpkg"))))
      }

      if(i == "florabr"){
        return(x %in% flora$species)
      }

      if(i == "faunabr"){
        return(x %in% fauna$species)
      }

      if(i == "wcvp"){
        return(x %in% wcvp$species)
      }

      if(i == "iucn"){
        return(x %in% iucn$species)
      }

      })
    names(r) <- l
    # Which databases available for species x
    avx <- names(r[unlist(r)])
    dr <- data.frame(species = x,
                     datasets = paste(avx, collapse = ";"))
    return(dr)
  }) #End of r_species

  # Dataset available for each species
  r_species <- as.data.frame(data.table::rbindlist(r_species))

  # If return distribution...
  if(return_distribution){
    d_species <- lapply(species, function(x){
      #Datasets available
      avx <- unlist(strsplit(r_species$datasets[r_species == x], ";"))

      if(length(avx) > 0){
        spt <- lapply(avx, function(y){

          if(y == "bien"){
            sp_bien <- gsub(" ", "_", x)
            return(vect(file.path(data_dir, paste0("bien/", sp_bien, ".gpkg"))))
          }

          if(y == "florabr"){
            spt_flora <- florabr::get_spat_occ(data = flora, species = x,
                                               verbose = FALSE)[[1]]
            return(spt_flora)
          }

          if(y == "faunabr"){
            spt_fauna <- faunabr::fauna_spat_occ(data = fauna, species = x,
                                                 verbose = FALSE)[[1]]
            return(spt_fauna)
          }

          if(y == "wcvp"){
            w_x <- wcvp[wcvp$species == x]
            w_x <- m[m$Level3_cod %in% w_x$LEVEL3_COD,]
            return(w_x)
          }

          if(y == "iucn"){
            w_x <- iucn[iucn$species == x]
            # Get regions of occurrence (by level)
            country_level <- w_x$code[nchar(w_x$code) == 2]
            level3 <- w_x$code[nchar(w_x$code) == 3]
            countries_level3 <- dm$ISO_Code[dm$Level3_cod %in% level3]
            level4 <- w_x$code[nchar(w_x$code) > 3]
            countries_level4 <- dm$ISO_Code[dm$Level4_cod %in% level4]

            # Filter levels
            # Remove countries already in levels 3 or 4
            country_level[country_level %in% countries_level3 |
                            country_level %in% countries_level4] <- NA
            country_level <- na.omit(country_level)

            # Remove levels 3 inside level 4
            level3[countries_level3 %in% countries_level4] <- NA
            level3 <- na.omit(level3)

            # Subset map
            m_i <- m[m$ISO_Code %in% country_level |
                       m$Level3_cod %in% level3 |
                       m$Level4_cod %in% level4, ]
            m_i <- terra::aggregate(m_i)
            return(m_i)
          }

        })
        names(spt) <- avx
        return(spt)
      } else {spt <- NULL}

    })
      if(length(d_species) == 1){
      d_species <- unlist(d_species, recursive = FALSE)
    } else{
      names(d_species) <- species
    }
  }

  # Final dataset
  if(return_distribution){
    res <- list("species_info" = r_species,
                "species_range" = d_species)
    return(res)
  } else {
    return(r_species)
  }

}
