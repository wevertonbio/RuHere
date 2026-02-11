#' Store SpeciesLink credential
#'
#' @description
#' This function sets the IUCN API key as an environment variable for the
#' current R session. This key is required to obtain distributional data
#' from IUCN.
#'
#' @usage set_iucn_credentials(iucn_key, verbose = TRUE)
#'
#' @param iucn_key (character) your IUCN API key. See Details.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @details
#' To check your API key, visit: [https://api.iucnredlist.org/users/edit](https://api.iucnredlist.org/users/edit).
#'
#' @returns
#' No return value.
#'
#' @importFrom utils file.edit
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_iucn_credentials(iucn_key = "my_key")
#' }
#'
set_iucn_credentials <- function(iucn_key, verbose = TRUE) {

  # Check arguments
  if (!inherits(iucn_key, "character"))
    stop("'iucn_key' must be a character, not ", class(iucn_key),
         call. = FALSE)

  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)

  # Set the key in the current session
  Sys.setenv(IUCN_REDLIST_KEY = iucn_key)

  if (verbose) {
    message("IUCN credentials set for the current session.")
    message("To make them permanent, manually add this line to your .Renviron file:")
    message(paste0("IUCN_REDLIST_KEY=", iucn_key))
  }

  invisible(NULL)
}
