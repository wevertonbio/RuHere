#' Store SpeciesLink credential
#'
#' @description
#' This function stores the IUCN credential (API key) in the R environment. This
#' API key is required to obtain distributional data from IUCN.
#'
#' @usage set_iucn_credentials(iucn_key, overwrite = FALSE,
#'                             open_Renviron = FALSE)
#'
#' @param iucn_key (character) your IUCN API key. See Details.
#' @param overwrite (logical) whether to overwrite IUCN credential if it
#' already exists. Default is FALSE.
#' @param open_Renviron (logical) whether to open the .Renviron file after
#' saving the credentials. Default is FALSE.
#'
#' @details
#' To check your API key, visit: [https://api.iucnredlist.org/users/edit](https://api.iucnredlist.org/users/edit).
#'
#'
#' @returns
#' If `open_Renviron` is set to TRUE, it opens the .Renviron file. Otherwise,
#' the credentials are saved silently.
#' @export
#'
#' @examples
#' \dontrun{
#' set_iucn_credentials(iucn_key = "my_key")
#' }
#'
set_iucn_credentials <- function(iucn_key,
                                 overwrite = FALSE, open_Renviron = FALSE) {

  # Check arguments
  if (!inherits(iucn_key, "character"))
    stop("'iucn_key' must be a character, not ", class(iucn_key),
         call. = FALSE)

  if (!inherits(overwrite, "logical") || length(overwrite) != 1)
    stop("'overwrite' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)

  if (!inherits(open_Renviron, "logical") || length(open_Renviron) != 1)
    stop("'open_Renviron' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)

  # Get R environment
  renviron_path <- file.path(Sys.getenv("HOME"), ".Renviron")

  # Reload the .Renviron file
  readRenviron(renviron_path)

  # Create R environment, if necessary
  if (file.exists(renviron_path)) {
    lines <- readLines(renviron_path, warn = FALSE)
  } else {
    lines <- c()
  }

  # Check if variables exist
  iucn_exists <- any(startsWith(lines, paste0("IUCN_REDLIST_KEY", "=")))

  if(iucn_exists){
    if(!overwrite){
      stop("IUCN_REDLIST_KEY already exists.
Check your .Renviron file or set 'overwrite = TRUE")
    } else {
      warning("Overwriting 'IUCN_REDLIST_KEY' in .Renviron")
      # Check lines to overwrite
      to_overwrite <- grepl("^IUCN_REDLIST_KEY",
                            lines)
      lines <- lines[!to_overwrite]
    }
  }

  new_lines <- c(lines,
                 paste0("IUCN_REDLIST_KEY=", "'", iucn_key, "'"))

  # Write lines
  writeLines(new_lines, renviron_path)

  # Reload the .Renviron file
  readRenviron(renviron_path)

  message("IUCN credentials have been processed and added/updated in your .Renviron file\n",
          "Check your .Renviron with file.edit('", normalizePath(renviron_path, winslash = "/"), "')")

  if(open_Renviron){
    file.edit(normalizePath(renviron_path, winslash = "/"))
  }
}

# set_iucn_credentials(iucn_key = "teste",
#                      overwrite = FALSE, open_Renviron = TRUE)

