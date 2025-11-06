#' Download the latest version of Flora e Funga do Brasil database
#'
#' @description
#' This function downloads the Flora e Funga do Brasil database, which is
#' required for filtering occurrence records using specialists' information
#' via the `flag_florabr()` function.
#'
#' @param data_dir (character) a directory to save the data downloaded from
#' Flora e Funga do Brasil.
#' @param data_version (character) version of the Flora e Funga do Brasil
#' database to download. Use "latest" to get the most recent version, updated
#' weekly. Alternatively, specify an older version (e.g.,
#' data_version="393.319"). Default value is "latest".
#' @param solve_discrepancy (logical) whether to resolve discrepancies between
#' species and subspecies/varieties information. When set to TRUE, species
#' information is updated based on unique data from varieties and subspecies.
#' For example, if a subspecies occurs in a certain biome, it implies that the
#' species also occurs in that biome. Default is TRUE.
#' @param overwrite (logical) if TRUE, data is overwritten. Default = TRUE.
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
#' @importFrom florabr get_florabr
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a directory to save the data
#' data_dir <- tempdir() # Here, a temporary directory
#'
#' # Download the latest version of the Flora e Funga do Brasil database
#' florabr_here(data_dir = data_dir)
#' }

florabr_here <- function(data_dir,
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
  dir.create(file.path(data_dir, "florabr"))

  if(verbose){
    message("Getting data from Flora e Funga do Brasil...")
  }
  florabr::get_florabr(output_dir = file.path(data_dir, "florabr"),
                       data_version,
                       solve_discrepancy = solve_discrepancy,
                       overwrite = overwrite,
                       verbose = verbose,
                       remove_files = remove_files)
  message("Data sucessfuly saved in ", file.path(data_dir, "florabr"))

}
