#' Store SpeciesLink credential
#'
#' @description
#' This function sets the SpeciesLink API key as an environment variable for
#' the current R session. This API key is required to retrieve occurrence records from
#' SpeciesLink.
#'
#' @usage set_specieslink_credentials(specieslink_key, verbose = TRUE)
#'
#' @param specieslink_key (character) your SpeciesLink API key.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @details
#' To check your API key, visit: [https://specieslink.net/aut/profile/apikeys](https://specieslink.net/aut/profile/apikeys).
#'
#' @returns
#' No return value.
#'
#' @importFrom utils file.edit
#'
#' @export
#' @examples
#' \dontrun{
#' set_specieslink_credentials(specieslink_key = "my_key")
#' }
#'
set_specieslink_credentials <- function(specieslink_key, verbose = TRUE) {
  # Check arguments
  if (!inherits(specieslink_key, "character") || length(specieslink_key) != 1)
    stop("'specieslink_key' must be a character, not ", class(specieslink_key),
         call. = FALSE)

  if (!is.logical(verbose) || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # Set the key in the current session
  Sys.setenv(SPECIESLINK_KEY = specieslink_key)

  if (verbose) {
    message("SpeciesLink credentials set for the current session.")
    message("To make them permanent, manually add this line to your .Renviron file:")
    message(paste0("SPECIESLINK_KEY=", specieslink_key))
  }

  invisible(NULL)
}
