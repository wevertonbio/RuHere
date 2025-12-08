#' Download species distribution information from BIEN
#'
#' @description
#' This function downloads distribution information from the BIEN database,
#' required for filtering occurrence records using specialists' information via
#' the `flag_bien()` function.
#'
#' @param data_dir (character) directory to save the data downloaded from BIEN.
#' @param species (character) a vector of species names for which to retrieve
#' distribution information.
#' @param synonyms (data.frame) an optional data.frame containing synonyms of
#' the target species. The first column must contain the target species names,
#' and the second column their corresponding synonyms. Default is `NULL`.
#' See details for more information.
#' @param overwrite (logical) whether to overwrite existing files. Default is
#' `TRUE`.
#' @param progress_bar (logical) whether to display a progress bar during processing.
#'   If TRUE, the 'pbapply' package must be installed. Default is `FALSE`.
#' @param verbose (logical) whether to display progress messages. Default is
#' `TRUE`.
#'
#' @details
#' This function uses the `BIEN::BIEN_ranges_load_species()` function to
#' retrieve polygons representing the distribution ranges of species available
#' in the BIEN database.
#'
#' Because taxonomic information in BIEN may be outdated, you can optionally
#' provide a table of synonyms to broaden the search. The synonyms data.frame
#' should have the accepted species in the first column and their synonyms in
#' the second. See `RuHere::synonys` for an example.
#'
#'
#' @returns
#' A data frame indicating whether the polygon(s) representing the species range
#' are available in BIEN.
#' If the range is available, a GeoPackage file (.gpkg) is saved in
#' `data_dir/bien`. The file name corresponds to the species name, with an
#' underscore (“_”) replacing the space between the genus and the specific
#' epithet.
#'
#' @importFrom BIEN BIEN_ranges_load_species
#' @importFrom terra vect writeVector
#' @importFrom data.table rbindlist
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a directory to save the data
#' data_dir <- tempdir() # Here, a temporary directory
#'
#' # Download species distribution information from BIEN
#' bien_here(data_dir = data_dir, species = "Handroanthus serratifolius")
#' }
bien_here <- function(data_dir,
                      species,
                      synonyms = NULL,
                      overwrite = TRUE,
                      progress_bar = FALSE,
                      verbose = TRUE){

  # --- Argument checks ----------------------------------

  # data_dir
  if (!inherits(data_dir, "character") || length(data_dir) != 1) {
    stop("'data_dir' must be a single character string.", call. = FALSE)
  }

  # species
  if (missing(species) || !inherits(species, "character") || length(species) == 0) {
    stop("'species' must be a character vector with at least one species name.",
         call. = FALSE)
  }

  # synonyms
  if (!is.null(synonyms)) {

    if (!inherits(synonyms, "data.frame")) {
      stop("'synonyms' must be a data.frame with two columns (accepted, synonym).",
           call. = FALSE)
    }

    if (ncol(synonyms) < 2) {
      stop("'synonyms' must have at least two columns: accepted_name and synonym.",
           call. = FALSE)
    }

    if (!inherits(synonyms[[1]], "character") ||
        !inherits(synonyms[[2]], "character")) {
      stop("The first two columns of 'synonyms' must be character vectors.",
           call. = FALSE)
    }
  }

  # overwrite
  if (!inherits(overwrite, "logical") || length(overwrite) != 1) {
    stop("'overwrite' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  # progress_bar
  if (!inherits(progress_bar, "logical") || length(progress_bar) != 1) {
    stop("'progress_bar' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  # verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  # Ensure data_dir exists
  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }

  # Create directory
  odir <- file.path(data_dir, "bien")
  dir.create(odir, showWarnings = FALSE)

  # Set progressbar
  if (progress_bar) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      my_lapply <- pbapply::pblapply
    } else {
      stop("Package 'pbapply' is required if 'progress_bar = TRUE'.
Run install.packages('pbapply')", call. = FALSE)
    }
  } else {
    my_lapply <- base::lapply
  }


  # Save species
  res <- my_lapply(species, function(i){
    if(!is.null(synonyms)){
      s_i <- synonyms[synonyms[[1]] == i, 2]
      if(length(s_i) > 0){
        spp <- unique(c(i, s_i))
      } else {spp <- i}
    } else {spp <- i}


    sp_i <- sub(" ", "_", spp)
    v_i <- BIEN::BIEN_ranges_load_species(species = sp_i, fetch.query = FALSE)
    if(nrow(v_i) > 0){
      v_i <- terra::vect(v_i)
      #Spp name can't have present spaces
      spp_name <- gsub(" ", "_", i)
      terra::writeVector(v_i, file.path(odir,
                                        paste0(spp_name, ".gpkg")),
                         overwrite = overwrite)
      return(data.frame(species = i, range_available = TRUE))
    } else {return(data.frame(species = i, range_available = FALSE))}
  })
  return(data.table::rbindlist(res))
}
