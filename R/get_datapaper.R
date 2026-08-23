#' Download and merge biodiversity data papers
#'
#' @description
#' Downloads, reads, and merges published biodiversity data papers (see details
#' for available options).
#' The function allows downloading single or multiple datasets simultaneously,
#' optionally filtering records by species name and saving the consolidated
#' table to disk in different file formats.
#'
#' @param datapaper (character) vector with the names of the data papers to
#' download. Valid options are: `"atlantic"`, `"brazil road-kill"`, `"dryflor"`,
#' `"neotropical xenarthrans"`, and `"neotroptree"`.
#' @param species (character) optional species name or vector of species names
#' to filter the dataset. Default is `NULL` (returns all species).
#' @param save (logical) whether to save the merged output file to disk. If
#' `FALSE`, files are downloaded to a temporary session directory. Default is
#' `FALSE`.
#' @param dir (character) directory path where the output file will be saved if
#' `save = TRUE`. Default is `NULL`.
#' @param file.format (character) file format to save the merged table. Valid
#' options are `"gz"` (compressed CSV), `"csv"`, or `"rds"`. Default is `"gz"`
#' @param overwrite (logical) whether to overwrite existing downloaded files in
#' the destination directory. Default is `FALSE`
#' @param verbose (logical) whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently. Default
#' is TRUE.
#'
#' @details
#' Available datasets that can be requested in \code{datapaper}:
#' \itemize{
#'   \item \code{"atlantic"}: Atlantic data papers compilations.
#'   \item \code{"brazil road-kill"}: Brazil road-kill dataset.
#'   \item \code{"dryflor"}: Latin American and Caribbean Seasonally Dry
#'   Tropical Forests (DryFlor) dataset.
#'   \item \code{"neotropical xenarthrans"}: Neotropical Xenarthrans occurrence
#'   dataset.
#'   \item \code{"neotroptree"}: NeoTropTree tree species checklist database.
#' }
#'
#' @note
#' When using any dataset retrieved by this function in publications or research
#' reports, please cite the original authors. Use the function
#' \code{cite_datapaper()} to obtain the complete bibliographic references.
#'
#' @returns
#' A \code{data.table} containing the consolidated records from the selected
#' data papers.
#'
#' @importFrom httr GET write_disk
#' @importFrom data.table fread rbindlist fwrite `%chin%`
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' # Download Atlantic dataset
#' res <- get_datapaper(datapaper = "atlantic")
#'
#' # Download multiple datasets and filter by a specific species
#' res_sp <- get_datapaper(
#'   datapaper = c("atlantic", "dryflor"),
#'   species = "Araucaria angustifolia")
#' }
get_datapaper <- function(datapaper, species = NULL, save = FALSE, dir = NULL,
                          file.format = "gz", overwrite = FALSE,
                          verbose = TRUE){

  # Argument checking... ####
  valid_datapapers <- c(
    "atlantic",
    "brazil road-kill",
    "dryflor",
    "neotropical xenarthrans",
    "neotroptree"
  )
  valid_formats <- c("gz", "csv", "rds", ".rds")

  # Check: 'datapaper' is provided and is a character vector
  if (missing(datapaper) || is.null(datapaper) || !is.character(datapaper)) {
    stop("The 'datapaper' argument is required and must be a character vector.",
         call. = FALSE)
  }

  # Check: all values in 'datapaper' match supported datasets
  invalid_datapapers <- setdiff(datapaper, valid_datapapers)
  if (length(invalid_datapapers) > 0) {
    stop(
      paste0(
        "Invalid option(s) in 'datapaper': ",
        paste(paste0("'", invalid_datapapers, "'"), collapse = ", "), ".\n",
        "Valid options are: ",
        paste(paste0("'", valid_datapapers, "'"), collapse = ", "), "."
      ),
      call. = FALSE
    )
  }

  # Check: 'species' must be character or NULL
  if (!is.null(species) && !is.character(species)) {
    stop("The 'species' argument must be a character vector or NULL.",
         call. = FALSE)
  }

  # Check: 'save' and 'overwrite' must be a single logical value (TRUE / FALSE)
  if (!is.logical(save) || length(save) != 1 || is.na(save)) {
    stop("The 'save' argument must be a single logical value: TRUE or FALSE.",
         call. = FALSE)
  }
  if (!is.logical(overwrite) || length(overwrite) != 1 || is.na(overwrite)) {
    stop("The 'overwrite' argument must be a single logical value: TRUE or FALSE.",
         call. = FALSE)
  }

  # Check: 'dir' must be provided when 'save = TRUE'
  if (save) {
    if (is.null(dir) || !is.character(dir) || length(dir) != 1 || nchar(dir) == 0) {
      stop("When 'save = TRUE', you must provide a valid directory path in 'dir'.",
           call. = FALSE)
    }
  }

  # Check: 'file.format' must match accepted formats
  if (!is.character(file.format) || !(file.format %in% valid_formats)) {
    stop(
      paste0(
        "Invalid file format: '", file.format, "'.\n",
        "Valid options for 'file.format' are: 'gz', 'csv', or 'rds'."
      ),
      call. = FALSE
    )
  }

  if(!save){
    dir <- file.path(tempdir(), "datapaper")
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Create list
  l <- vector("list", length(datapaper))
  names(l) <- datapaper

  # Download
  if("atlantic" %in% datapaper){
    if(verbose) message("Downloading data from atlantic datapapers...")

    httr::GET("https://zenodo.org/records/21986019/files/atlantic_data_papers.gz?download=1",
              httr::write_disk(file.path(file.path(dir, "atlantic.gz")),
                               overwrite = T))
    l[["atlantic"]] <- file.path(dir, "atlantic.gz")
  }

  if("brazil road-kill" %in% datapaper){
    if(verbose) message("Downloading data from Brazil road-kill...")
    httr::GET("https://zenodo.org/records/21986019/files/Brazil_road-kill_RuHere.gz?download=1",
              httr::write_disk(file.path(file.path(dir, "brazil_road_kill.gz")),
                               overwrite = T))
    l[["brazil road-kill"]] <- file.path(dir, "brazil_road_kill.gz")
  }

  if("dryflor" %in% datapaper){
    if(verbose) message("Downloading data from DryFlor...")
    httr::GET("https://zenodo.org/records/21986019/files/DryFlor_RuHere.gz?download=1",
              httr::write_disk(file.path(file.path(dir, "dryflor.gz")),
                               overwrite = T))
    l[["dryflor"]] <- file.path(dir, "dryflor.gz")
  }

  if("neotropical xenarthrans" %in% datapaper){
    if(verbose) message("Downloading data from Neotropical Xenarthrans...")
    httr::GET("https://zenodo.org/records/21986019/files/Neotropical_Xenarthrans_RuHere.gz?download=1",
              httr::write_disk(file.path(file.path(dir, "neotropical_xenarthrans.gz")),
                               overwrite = T))
    l[["neotropical xenarthrans"]] <- file.path(dir, "neotropical_xenarthrans.gz")
  }

  if("neotroptree" %in% datapaper){
    if(verbose) message("Downloading data from NeoTropTree...")
    httr::GET("https://zenodo.org/records/21986019/files/NeoTropTree_RuHere.gz?download=1",
              httr::write_disk(file.path(file.path(dir, "neotroptree.gz")),
                               overwrite = T))
    l[["neotroptree"]] <- file.path(dir, "neotroptree.gz")
  }

  # Read files
  if(length(l) > 1){
    d <- data.table::rbindlist(
      lapply(l, data.table::fread), fill = TRUE
    )
  } else if(length(l) == 1){
    d <- data.table::fread(l[[1]])
  }

  # Subset species?
  if(!is.null(species)){
    if(verbose) message("Subsetting species...")
    sp_alvo <- species
    d <- d[data.table::`%chin%`(species, sp_alvo)]
    if(nrow(d) == 0){
      warning("None of the specified species are present in the datapaper(s). Returning an empty table.")
    }
  }

  # Save?
  if(save && nrow(d) > 0){

    if (file.format == "gz"){
      file.name <- file.path(dir, "datapaper.gz")
      data.table::fwrite(d, file = file.name)
    } else if (file.format == "csv"){
      file.name <- file.path(dir, "datapaper.csv")
      data.table::fwrite(d, file = file.name)
    } else if (file.format == "rds"){
      file.name <- file.path(dir, "datapaper.rds")
      saveRDS(d, file = file.name)
    }
  }

  # Print message
  message(
    "Datapaper(s) successfully retrieved.\n\n",
    "##########################################################################\n",
    "  PLEASE, USE THE FUNCTION cite_datapaper() TO CITE DATAPAPERS RETRIEVED  \n",
    "##########################################################################"
  )

  # Return data.frame
  return(d)

  }


