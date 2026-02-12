#' Store SpeciesLink credential
#'
#' @description
#' This function sets the IUCN API key as an environment variable in the R
#' environment. This key is required to obtain distributional data from IUCN.
#'
#' @param iucn_key (character) your IUCN API key. See Details.
#' @param permanently (logical) whether to add the SpeciesLink API key
#' permanently to the R environment. Default is `FALSE`, meaning it will be
#' added only temporarily for the current session.
#' @param overwrite (logical) whether to overwrite IUCN credential if it
#' already exists. Only applicable if `permanently` is set to `TRUE`. Default is
#' `FALSE`.
#' @param open_Renviron (logical) whether to open the .Renviron file after
#' saving the credential. Only applicable if `permanently` is set to `TRUE`.
#' Default is `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @details
#' To check your API key, visit: [https://api.iucnredlist.org/users/edit](https://api.iucnredlist.org/users/edit).
#'
#' @returns
#' If `permanently` and `open_Renviron` are set to TRUE, it opens the .Renviron
#' file. Otherwise, the credentials are saved silently.
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
set_iucn_credentials <- function(iucn_key, permanently = FALSE,
                                 overwrite = FALSE, open_Renviron = FALSE,
                                 verbose = TRUE) {

  # Check arguments
  if (!inherits(iucn_key, "character"))
    stop("'iucn_key' must be a character, not ", class(iucn_key),
         call. = FALSE)

  if (!is.logical(permanently) || length(permanently) != 1) {
    stop("'permanently' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)

  if(permanently){
    if (!inherits(overwrite, "logical") || length(overwrite) != 1)
      stop("'overwrite' must be a single logical value (TRUE or FALSE).",
           call. = FALSE)
    if (!inherits(open_Renviron, "logical") || length(open_Renviron) != 1)
      stop("'open_Renviron' must be a single logical value (TRUE or FALSE).",
           call. = FALSE)
  }

  # Set the key in the current session
  Sys.setenv(IUCN_REDLIST_KEY = iucn_key)

  if(!permanently && verbose){
  message("IUCN credentials set temporarily only for the current session.
To make it permanent, set 'permanently = TRUE'")
    }

  # If add permanently...
  if(permanently){
    # Get R environment
    renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

    # Create R environment, if necessary
    if (file.exists(renviron_path)) {
      lines <- readLines(renviron_path, warn = FALSE)
    } else {
      lines <- character(0)
    }

    # Check if variables exist
    iucn_exists <- any(startsWith(lines, paste0("IUCN_REDLIST_KEY", "=")))

    if(iucn_exists){
      if(!overwrite){
        stop("IUCN_REDLIST_KEY already exists. Check your .Renviron file or set 'overwrite = TRUE")
      } else {
        warning("Overwriting 'IUCN_REDLIST_KEY' in .Renviron")
        # Check lines to overwrite
        to_overwrite <- grepl("^IUCN_REDLIST_KEY",
                              lines)
        lines <- lines[!to_overwrite]
      }
    }

    new_lines <- c(lines,
                   paste0("IUCN_REDLIST_KEY=", iucn_key))

    # Write lines
    writeLines(new_lines, renviron_path)

    if (verbose) message("IUCN credentials have been processed and added/updated in your .Renviron file\n",
                         "Check your .Renviron with file.edit('", normalizePath(renviron_path, winslash = "/"), "')")

    if(open_Renviron){
      utils::file.edit(normalizePath(renviron_path, winslash = "/"))
    }

  } #End of permanently


  invisible(NULL)
}
