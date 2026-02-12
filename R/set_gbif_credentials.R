#' Store GBIF credentials
#'
#' @description
#' This function sets GBIF credentials (username, email and password) as
#' environment variables in the R environment. These credentials are required to
#' retrieve occurrence records from GBIF.
#'
#' @param gbif_username (character) your GBIF username.
#' @param gbif_email (character) your GBIF email address.
#' @param gbif_password (character) your GBIF password.
#' @param permanently (logical) whether to add the GBIF credentials permanently
#' to the R environment. Default is `FALSE`, meaning it will be added only
#' temporarily for the current session.
#' @param overwrite (logical) whether to overwrite GBIF credentials if they
#' already exist. Only applicable if permanently is set to `TRUE`. Default is
#' `FALSE`.
#' @param open_Renviron (logical) whether to open the .Renviron file after
#' saving the credentials. Only applicable if permanently is set to `TRUE`.
#' Default is `FALSE`.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @returns
#' If `permanently` and `open_Renviron` are set to TRUE, it opens the .Renviron
#' file. Otherwise, the credentials are saved silently.
#'
#' @importFrom utils file.edit
#' @export
#'
#' @examples
#' \dontrun{
#' set_gbif_credentials(gbif_username = "my_username",
#'                      gbif_email = "my_email@example.com",
#'                      gbif_password = "my_password")
#' }
set_gbif_credentials <- function(gbif_username, gbif_email, gbif_password,
                                 permanently = FALSE, overwrite = FALSE,
                                 open_Renviron = FALSE,
                                 verbose = TRUE) {

  # Check arguments
  if (!inherits(gbif_username, "character"))
    stop("'gbif_username' must be a character, not ", class(gbif_username),
         call. = FALSE)

  if (!inherits(gbif_email, "character"))
    stop("'gbif_email' must be a character, not ", class(gbif_email),
         call. = FALSE)

  if (!inherits(gbif_password, "character"))
    stop("'gbif_password' must be a character, not ", class(gbif_password),
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
  Sys.setenv(GBIF_USER = gbif_username)
  Sys.setenv(GBIF_EMAIL = gbif_email)
  Sys.setenv(GBIF_PWD = gbif_password)

  if(!permanently && verbose){
      message("GBIF credentials set temporarily for the current session.
To make them permanent, set 'permanently = TRUE'")
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
    user_exists <- any(startsWith(lines, paste0("GBIF_USER", "=")))
    email_exists <- any(startsWith(lines, paste0("GBIF_EMAIL", "=")))
    pwd_exists <- any(startsWith(lines, paste0("GBIF_PWD", "=")))

    if(any(c(user_exists, email_exists, pwd_exists))){
      if(!overwrite){
        stop("gbif_username, email and/or pwd already exists. Check your .Renviron file or set 'overwrite = TRUE")
      } else {
        if (verbose) {
          warning("Overwriting gbif_username, email and pwd in .Renviron")
          # Check lines to overwrite
          to_overwrite <- grepl("^(GBIF_USER|GBIF_EMAIL|GBIF_PWD)",
                                lines)
          lines <- lines[!to_overwrite]
        }
      }
    }

    # Add/update lines
    update_lines <- paste0("GBIF_USER=", gbif_username, "\n",
                           "GBIF_EMAIL=", gbif_email, "\n",
                           "GBIF_PWD=", gbif_password)
    new_lines <- c(lines,
                   paste0("GBIF_USER=", gbif_username),
                   paste0("GBIF_EMAIL=", gbif_email),
                   paste0("GBIF_PWD=", gbif_password))

    # Write lines
    writeLines(new_lines, renviron_path)

    if (verbose) {
      message("GBIF credentials have been processed and added/updated in your .Renviron file\n",
              "Check your .Renviron with file.edit('", normalizePath(renviron_path, winslash = "/"), "')")
    }

    if(open_Renviron){
      utils::file.edit(normalizePath(renviron_path, winslash = "/"))
    }
  } #End of permanently

  invisible(NULL)

  }
