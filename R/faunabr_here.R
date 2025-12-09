#' Download the latest version of the Fauna do Brazil (Taxonomic Catalog of the
#' Brazilian Fauna)
#'
#' @description
#' This function downloads the Taxonomic Catalog of the Brazilian Fauna
#' database, which is required for filtering occurrence records using
#' specialists' information via the `flag_faunabr()` function.
#'
#' @param data_dir (character) a directory to save the data downloaded from
#' Fauna do Brazil.
#' @param data_version (character) version of the Fauna do Brazil database to
#' download. Use "latest" to get the most recent version, which is updated
#' frequently. Alternatively, specify an older version (e.g.,
#' data_version="1.2"). Default value is "latest".
#' @param solve_discrepancy (logical) whether to resolve inconsistencies
#' between species and subspecies information. When set to TRUE (default),
#' species information is updated based on unique data from subspecies. For
#' example, if a subspecies occurs in a certain state, it implies that the
#' species also occurs in that state.
#' @param overwrite (logical) If TRUE, data is overwritten. Default is TRUE.
#' @param remove_files (logical) whether to remove the downloaded files used in
#' building the final dataset. Default is TRUE.
#' @param verbose (logical) whether to display messages during function
#' execution. Set to TRUE to enable display, or FALSE to run silently. Default
#' is TRUE.
#'
#' @returns
#' A message indicating that the data were successfully saved in the directory
#' specified by `data_dir`.
#'
#' @importFrom faunabr get_faunabr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a directory to save the data
#' data_dir <- tempdir() # Here, a temporary directory
#'
#' # Download the latest version of the Flora e Funga do Brazil database
#' faunabr_here(data_dir = data_dir)
#' }
faunabr_here <- function(data_dir,
                         data_version = "latest",
                         solve_discrepancy = TRUE,
                         overwrite = TRUE,
                         remove_files = TRUE,
                         verbose = TRUE){

  # Check arguments -----------------------------------------------------------

  # data_dir
  if (!inherits(data_dir, "character") || length(data_dir) != 1)
    stop("`data_dir` must be a single character string indicating the directory path.")

  # data_version
  if (!inherits(data_version, "character") || length(data_version) != 1)
    stop("`data_version` must be a single character string (e.g., 'latest' or '393.319').")

  # solve_discrepancy
  if (!inherits(solve_discrepancy, "logical") || length(solve_discrepancy) != 1)
    stop("`solve_discrepancy` must be a single logical value (TRUE or FALSE).")

  # overwrite
  if (!inherits(overwrite, "logical") || length(overwrite) != 1)
    stop("`overwrite` must be a single logical value (TRUE or FALSE).")

  # remove_files
  if (!inherits(remove_files, "logical") || length(remove_files) != 1)
    stop("`remove_files` must be a single logical value (TRUE or FALSE).")

  # verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("`verbose` must be a single logical value (TRUE or FALSE).")


  if(!file.exists(data_dir)){
    stop(data_dir, " directory does not exist. Please create it or specify a
         different directory.")
  }


  # Create directory
  dir.create(file.path(data_dir, "faunabr"))

  if(verbose){
    message("Getting data from Taxonomic Catalog of the Brazilian Fauna ...")
  }
  faunabr::get_faunabr(output_dir = file.path(data_dir, "faunabr"),
                       data_version,
                       solve_discrepancies = solve_discrepancy,
                       overwrite = overwrite,
                       verbose = verbose)
  # Remove files
  if (remove_files) {
    to_remove <- list.files(file.path(data_dir, "faunabr"), full.names = TRUE,
                            recursive = TRUE)
    to_remove <- to_remove[!grepl("CompleteBrazilianFauna",
                                  to_remove, fixed = TRUE)]
    try(invisible(unlink(to_remove, recursive = TRUE, force = TRUE)))
  }

  message("Data sucessfuly saved in ", file.path(data_dir, "faunabr"))

}
