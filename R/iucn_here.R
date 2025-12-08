#' Download species distribution information from IUCN
#'
#' @description
#' This function downloads information on species distributions from the IUCN
#' Red List, required for filtering occurrence records using specialists'
#' information via the `flag_iucn()` function.
#'
#' @param data_dir (character) directory to save the data downloaded from
#' IUCN.
#' @param species (character) a vector of species names for which to retrieve
#' distribution information.
#' @param synonyms (data.frame) an optional data.frame containing synonyms of
#' the target species. The first column must contain the target species names,
#' and the second column their corresponding synonyms. Default is `NULL`.
#' See details for more information.
#' @param iucn_credential (character) your IUCN API key. Default is `NULL`, in
#' which case the function will attempt to read the API key from your R
#' environment. You can set it in advance using the `set_iucn_credentials()`
#' function.
#' @param overwrite (logical) whether to overwrite existing files. Default is
#' `TRUE`.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) whether to display progress messages. Default is
#' `FALSE`.
#' @param return_data (logical) whether to return a data frame containing the
#' species distribution information downloaded from IUCN. Default is `TRUE`.
#'
#' @details
#' This function uses the `rredlist::rl_species()` function to retrieve
#' distribution data from the IUCN Red List. The data include information at
#' the country and regional levels, following the World Geographical Scheme for
#' Recording Plant Distributions (WGSRPD) — but applicable to both plants and
#' animals.
#'
#' Unfortunately, the range polygons available at
#' [https://www.iucnredlist.org/resources/spatial-data-download](https://www.iucnredlist.org/resources/spatial-data-download)
#' cannot be accessed automatically.
#'
#' Because taxonomic information in IUCN may be outdated, you can optionally
#' provide a table of synonyms to broaden the search. The synonyms data.frame
#' should have the accepted species in the first column and their synonyms in
#' the second. See `RuHere::synonys` for an example.
#'
#' The function also downloads the WGSRPD map used to represent distribution
#' regions.
#'
#' @returns
#' A message indicating that the data were successfully saved in the directory
#' specified by `data_dir`.
#' If `return_data = TRUE`, the function additionally returns a data frame
#' containing the species distribution information retrieved from IUCN.
#'
#' @importFrom rredlist rl_species rl_assessment
#' @importFrom data.table rbindlist fwrite
#' @importFrom stats na.omit
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a directory to save the data
#' data_dir <- tempdir() # Here, a temporary directory
#'
#' # Download species distribution information from IUCN
#' iucn_here(data_dir = data_dir, species = "Araucaria angustifolia")
#' }
iucn_here <- function(data_dir,
                      species,
                      synonyms = NULL,
                      iucn_credential = NULL,
                      overwrite = TRUE,
                      progress_bar = FALSE,
                      verbose = FALSE,
                      return_data = TRUE){

  # ---- ARGUMENT CHECKING ----

  # 1. Check species
  if (missing(species) || !is.character(species) || length(species) == 0) {
    stop("'species' must be provided as a non-empty character vector.",
         call. = FALSE)
  }

  # 2. Check synonyms
  if (!is.null(synonyms)) {
    if (!inherits(synonyms, "data.frame")) {
      stop("'synonyms' must be a data.frame with at least two columns (species, synonyms), or NULL.",
           call. = FALSE)
    }
    if (ncol(synonyms) < 2) {
      stop("'synonyms' must have at least two columns: the first with species names and the second with their synonyms.",
           call. = FALSE)
    }
  }

  # 3. Check data_dir
  if (missing(data_dir) || !is.character(data_dir) || length(data_dir) != 1) {
    stop("'data_dir' must be a single character string specifying a valid directory.",
         call. = FALSE)
  }
  if(!dir.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a different directory.")
  }

  # 4. Check iucn_credential
  if (!is.null(iucn_credential) && (!is.character(iucn_credential) || length(iucn_credential) != 1)) {
    stop("'iucn_credential' must be a single character string or NULL.",
         call. = FALSE)
  }
  # Check iucn api
  if (is.null(iucn_credential)) {
    key <- Sys.getenv("IUCN_REDLIST_KEY")
  } else {
    key <- iucn_credential
  }

  if(key == ""){
    stop("You must get and save an IUCN API key. Check the function 'set_iucn_credentials()")
  }

  # 5. Check overwrite
  if (!is.logical(overwrite) || length(overwrite) != 1) {
    stop("'overwrite' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # 6. Check verbose
  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # 7. Check return_data
  if (!is.logical(return_data) || length(return_data) != 1) {
    stop("'return_data' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # progress_bar
  if (!inherits(progress_bar, "logical") || length(progress_bar) != 1) {
    stop("'progress_bar' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }


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


  # Create directory
  odir <- file.path(data_dir, "iucn")
  dir.create(odir, showWarnings = FALSE)

  # Get species information
  spinfo <- my_lapply(species, function(i){
    if(!is.null(synonyms)){
      s_i <- synonyms[synonyms[[1]] == i, 2]
      if(length(s_i) > 0){
        spp <- unique(c(i, s_i))
      } else {spp <- i}
    } else {spp <- i}

    # Split binomial name in genus and species
    ss <- strsplit(spp, " ")
    names(ss) <- spp

    r <- lapply(ss, function(x){
      r <- try(rredlist::rl_species(genus = x[1],
                               species = x[2],
                               key = key)$assessments, silent = TRUE)

      if(inherits(r, "data.frame")){
        assessment <- rredlist::rl_assessment(r$assessment_id[r$latest],
                                              key = key)
        if("locations" %in% names(assessment)){
          l <- assessment$locations
          d <- l$description$en
          l <- l[, c("code", "origin", "presence")]
          l$species <- i
          l$region <- d
          l <- relocate_before(l, "species", names(l)[1])
          l <- relocate_after(l, "region", "species")

        } else { #If there is no info on location
          l <- data.frame(species = i, region = NA, code = NA, origin = NA,
                          presence = NA)
        }
      } else { #If it's not a dataframe
        l <- data.frame(species = i, region = NA, code = NA, origin = NA,
                        presence = NA)
      }
      return(l)
    })
    r <- stats::na.omit(data.table::rbindlist(r))
    return(r)
  })

  spinfo <- unique(data.table::rbindlist(spinfo))

  # Save results
  # Check if some file exists:
  file_exists <- file.exists(file.path(odir, "iucn_distribution.gz"))
  if(file_exists){
    if(!overwrite){
      stop("IUCN dataset already exists in the '", data_dir, "'.\n",
         "Set 'overwrite = TRUE' or change the directory specified in 'data_dir'")
    } else {
      warning("Appending new information to the existing IUCN dataset...")
      #Import existing dataset
      iucn_file <- data.table::fread(file.path(odir, "iucn_distribution.gz"))
      #Merge and get unique results
      spinfo <- unique(rbind(iucn_file, spinfo))
  }
  }

  # Save
  data.table::fwrite(x = spinfo,
                     file = file.path(odir, "iucn_distribution.gz"))

  # Get map
  utils::download.file(url = "https://zenodo.org/records/17455838/files/wgsrpd.gpkg?download=1",
                       destfile = file.path(odir, "wgsrpd.gpkg"),
                       method = "auto",
                       mode = "wb",
                       cacheOK = TRUE)

  # Data saved in...
  if(verbose){
    message("Data sucessfully saved in '", odir, "'")
  }

  # If return data...
  if(return_data){
    return(spinfo)
  } else {
    return(invisible(NULL))
  }

}
