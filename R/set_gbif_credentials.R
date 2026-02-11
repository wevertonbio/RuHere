#' Store GBIF credentials
#'
#' @description
#' This function sets GBIF credentials (username, email and password) as
#' environment variables for the current R session. These credentials are required
#' to retrieve occurrence records from GBIF.
#'
#' @details
#' To make these credentials permanent, you can manually add them to your
#' `.Renviron` file.
#'
#' @usage set_gbif_credentials(gbif_username, gbif_email, gbif_password,
#'                            verbose = TRUE)
#'
#' @param gbif_username (character) your GBIF username.
#' @param gbif_email (character) your GBIF email address.
#' @param gbif_password (character) your GBIF password.
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @returns
#' No return value.
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

  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("'verbose' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)

  # Set the key in the current session
  Sys.setenv(GBIF_USER = gbif_username)
  Sys.setenv(GBIF_EMAIL = gbif_email)
  Sys.setenv(GBIF_PWD = gbif_password)

  if (verbose) {
    message("GBIF credentials set for the current session.")
    message("To make them permanent, manually add these lines to your .Renviron file:")
    message(paste0("GBIF_USER=", gbif_username))
    message(paste0("GBIF_EMAIL=", gbif_email))
    message(paste0("GBIF_PWD=", gbif_password))
  }

  invisible(NULL)

  }
