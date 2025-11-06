#' Download distribution data from the World Checklist of Vascular Plants (WCVP)
#'
#' @description
#' This function downloads the World Checklist of Vascular Plants database,
#' which is required for filtering occurrence records using specialists'
#' information via the `flag_wcvp()` function.
#'
#'
#' @param data_dir (character) a directory to save the data downloaded from
#' WCVP.
#' @param overwrite (logical) If TRUE, data is overwritten. Default is TRUE.
#' @param remove_files (logical) whether to remove the downloaded files used in
#' building the final dataset. Default is TRUE.
#' @param timeout (numeric) maximum time (in seconds) allowed for downloading.
#' Default is 300. Slower internet connections may require higher values.
#' @param verbose (logical) whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently. Default
#' is TRUE.
#'
#' @returns
#' A message indicating that the data were successfully saved in the directory
#' specified by `data_dir`.
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a directory to save the data
#' data_dir <- tempdir() # Here, a temporary directory
#'
#' # Download the WCVP database
#' wcvp_here(data_dir = data_dir)
#' }
wcvp_here <- function(data_dir,
                      overwrite = TRUE,
                      remove_files = TRUE,
                      timeout = 300,
                      verbose = TRUE){

  # Check arguments
  if (!inherits(data_dir, "character"))
    stop("'data_dir' must be a character string indicating the directory path.", call. = FALSE)

  if (!dir.exists(data_dir))
    stop("'data_dir' does not exist. Please provide a valid and existing directory.", call. = FALSE)

  if (!inherits(overwrite, "logical") || length(overwrite) != 1)
    stop("'overwrite' must be a single logical value (TRUE or FALSE).", call. = FALSE)

  if (!inherits(remove_files, "logical") || length(remove_files) != 1)
    stop("'remove_files' must be a single logical value (TRUE or FALSE).", call. = FALSE)

  if (!inherits(timeout, "numeric") || length(timeout) != 1 || timeout <= 0)
    stop("'timeout' must be a single positive numeric value (in seconds).", call. = FALSE)

  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)


  # Create directory
  odir <- file.path(data_dir, "wcvp")
  dir.create(odir, showWarnings = FALSE)

  # Set time out
  original_timeout <- getOption("timeout")
  if(timeout != original_timeout){
    options(timeout = timeout)
  }

  if(verbose){
    message("Task 1 of 3: Downloading data from the World Checklist of Vascular Plants (WCVP) repository...\n")
  }
  # Downlod file
  utils::download.file(url = "https://sftp.kew.org/pub/data-repositories/WCVP/wcvp.zip",
                       destfile = file.path(odir, "wcvp.zip"),
                       method = "auto",
                       cacheOK = TRUE)


  if(verbose){
    message("Task 2 of 3: Merging data...\n")
  }

  # Unzip file
  utils::unzip(zipfile = file.path(odir, "wcvp.zip"),
               exdir = file.path(odir))

  wcp_names <- data.table::fread(file.path(odir, "wcvp_names.csv"),
                                 select = c("plant_name_id", "taxon_name"))

  colnames(wcp_names)[2] <- "species"
  wcp_dist <- data.table::fread(file.path(odir, "wcvp_distribution.csv"),
                                select = c("plant_name_id", "area_code_l3",
                                           "introduced", "extinct",
                                           "location_doubtful")) %>% distinct()
  colnames(wcp_dist)[2] <- "LEVEL3_COD"
  wcp <- left_join(wcp_names, wcp_dist, by = "plant_name_id") %>% na.omit() %>%
    dplyr::select(-plant_name_id)

  # Save results
  data.table::fwrite(wcp,
                     file.path(odir, "wcvp.gz"))

  if(verbose){
    message("Task 3 of 3: Downloading map from the World Geographical Scheme for Recording Plant Distributions (WGSRPD)...\n")
  }

  # Get map
  utils::download.file(url = "https://zenodo.org/records/17455838/files/wgsrpd.gpkg?download=1",
                       destfile = file.path(odir, "wgsrpd.gpkg"),
                       method = "auto",
                       mode = "wb",
                       cacheOK = TRUE)


  # Remove files
  if(remove_files){
    unlink(file.path(odir, "wcvp_names.csv"), recursive = TRUE, force = TRUE)
    unlink(file.path(odir, "wcvp_distribution.csv"), recursive = TRUE, force = TRUE)
    unlink(file.path(odir, "wcvp.zip"), recursive = TRUE, force = TRUE)
    unlink(file.path(odir, "README_WCVP.xlsx"), recursive = TRUE, force = TRUE)
  }


  # Set time out
  if(original_timeout != timeout){
    options(timeout = original_timeout)
  }

  if(verbose){
    message("Data sucessfuly saved in ", file.path(data_dir, "florabr\n"))
  }

  message("Please don't forget to cite:\n
Govaerts, R., Nic Lughadha, E. et al. The World Checklist of Vascular Plants, a continuously updated resource for exploring global plant diversity. Sci Data, 8, 215 (2021). https://doi.org/10.1038/s41597-021-00997-6")
}
