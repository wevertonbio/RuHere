#' Store SpeciesLink credential
#'
#' @description
#' This function stores the SpeciesLink credential (API key) in the R
#' environment. This API key is required to retrieve occurrence records from
#' SpeciesLink.
#'
#' @usage set_specieslink_credentials(specieslink_key, overwrite = FALSE,
#'                                    open_Renviron = FALSE)
#'
#' @param specieslink_key (character) your SpeciesLink API key.
#' @param overwrite (logical) whether to overwrite SpeciesLink credential if it
#' already exists. Default is FALSE.
#' @param open_Renviron (logical) whether to open the .Renviron file after
#' saving the credentials. Default is FALSE.
#'
#' @details
#' To check your API key, visit: [https://specieslink.net/aut/profile/apikeys](https://specieslink.net/aut/profile/apikeys).
#'
#' @returns
#' If `open_Renviron` is set to TRUE, it opens the .Renviron file. Otherwise,
#' the credentials are saved silently.
#'
#' @export
#' @examples
#' \dontrun{
#' set_specieslink_credentials(specieslink_key = "my_key")
#' }
#'
set_specieslink_credentials <- function(specieslink_key, overwrite = FALSE,
                                        open_Renviron = FALSE) {
  # Check arguments
  if (!inherits(specieslink_key, "character"))
    stop("'specieslink_key' must be a character, not ", class(specieslink_key),
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
  key_exists <- any(startsWith(lines, paste0("specieslink_key", "=")))


  if(key_exists){
    if(!overwrite){
      stop("specieslink_key already exists. Check your .Renviron file or set 'overwrite = TRUE")
    } else {
      warning("Overwriting variable specieslink_key in .Renviron")
      # Check lines to overwrite
      to_overwrite <- grepl("^specieslink_key",
                            lines)
      lines <- lines[!to_overwrite]
    }
  }

  # Add/update lines
  new_lines <- c(lines,
                 paste0("specieslink_key=", "'", specieslink_key, "'"))

  # Write lines
  writeLines(new_lines, renviron_path)

  # Reload the .Renviron file
  readRenviron(renviron_path)

  message("speciesLink credentials have been processed and added/updated in your .Renviron file\n",
          "Check your .Renviron with file.edit('", normalizePath(renviron_path, winslash = "/"), "')")

  if(open_Renviron){
    file.edit(normalizePath(renviron_path, winslash = "/"))
  }
}
