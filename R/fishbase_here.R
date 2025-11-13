#' @title fishbase_here
#'
#' @usage fishbase_here(data_dir, data_version = "latest", verbose = TRUE)
#'
#' @description
#' Downloads taxonomic and distribution tables from the **FishBase**
#' database using the \code{rfishbase} package and saves them locally in a
#' standardized directory structure.
#'
#' @param data_dir (character) the path to the local directory where a new
#' fishbase subdirectory will be created to store all downloaded data files. **Required.**
#' @param data_version (character) the specific version of FishBase data to query.
#' Default is `"latest"`.
#' @param verbose (logical) if `TRUE`, messages indicating the download
#' progress. Default is `TRUE`.
#'
#' @details
#' All files are saved compressed as `.gz` files within a new \code{fishbase}
#' subdirectory inside \code{data_dir}.
#'
#' @return
#' A message indicating that the data were successfully saved in the directory
#' specified by `data_dir`.
#'
#' @importFrom rfishbase species species_names country c_code
#' @importFrom dplyr left_join %>%
#' @importFrom data.table fwrite
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a directory to save the data
#' data_dir <- tempdir() # Here, a temporary directory
#'
#' # Download the latest version of the FishBase database
#' fishbase_here(data_dir = data_dir)
#' }
#'
fishbase_here <- function(data_dir,
                          data_version = "latest",
                          verbose = TRUE) {


  if (!inherits(data_dir, "character") || length(data_dir) != 1)
    stop("`data_dir` must be a single character string indicating the directory path.")

  if (!inherits(data_version, "character") || length(data_version) != 1)
    stop("`data_version` must be a single character string (e.g., 'latest').")

  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("`verbose` must be a single logical value (TRUE or FALSE).")

  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a
         different directory.")
  }

  dir.create(file.path(data_dir, "fishbase"))

  if(verbose) message("Getting data from FishBase...")

  sp_list <- rfishbase::species(version = data_version)
  names_sp <- rfishbase::species_names()
  comp_list <- sp_list %>%
    dplyr::left_join(names_sp, by = "SpecCode")

  data.table::fwrite(comp_list,
                     file.path(data_dir, "fishbase/fb_species_list.gz"))

  suppressMessages(fb_country <- rfishbase::country(comp_list$Species,
                                            version = data_version))
  data.table::fwrite(fb_country,
                     file.path(data_dir, "fishbase/fb_species_country.gz"))

  suppressMessages(fb_decoder <- rfishbase::c_code(version = data_version))
  data.table::fwrite(fb_decoder,
                     file.path(data_dir, "fishbase/fb_countries_decoder.gz"))

  message("Data successfully saved in ", file.path(data_dir, "fishbase"))

}
