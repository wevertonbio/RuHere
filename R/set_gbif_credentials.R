#' Store GBIF credentials
#'
#' @description
#' This function stores the GBIF credentials (username, email and password) in
#' the R environment. These credentials are required to retrieve occurrence
#' records from GBIF.
#'
#' @usage set_gbif_credentials(gbif_username, gbif_email, gbif_password,
#'                            overwrite = FALSE, open_Renviron = FALSE)
#'
#' @param gbif_username (character) your GBIF username.
#' @param gbif_email (character) your GBIF email adress.
#' @param gbif_password (character) your GBIF password.
#' @param overwrite (logical) whether to overwrite GBIF credentials if they
#' already exist. Default is FALSE.
#' @param open_Renviron (logical) whether to open the .Renviron file after
#' saving the credentials. Default is FALSE.
#'
#' @returns
#' If `open_Renviron` is set to TRUE, it opens the .Renviron file. Otherwise,
#' the credentials are saved silently.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' set_gbif_credentials(gbif_username = "my_username",
#'                      gbif_email = "my_email@example.com",
#'                      gbif_password = "my_password")
#' }
set_gbif_credentials <- function(gbif_username, gbif_email, gbif_password,
                                 overwrite = FALSE, open_Renviron = FALSE) {

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
  user_exists <- any(startsWith(lines, paste0("gbif_user", "=")))
  email_exists <- any(startsWith(lines, paste0("gbif_email", "=")))
  pwd_exists <- any(startsWith(lines, paste0("gbif_pwd", "=")))

  if(any(c(user_exists, email_exists, pwd_exists))){
    if(!overwrite){
      stop("gbif_username, email and/or pwd already exists. Check your .Renviron file or set 'overwrite = TRUE")
    } else {
      warning("Overwriting gbif_username, email and pwd in .Renviron")
      # Check lines to overwrite
      to_overwrite <- grepl("^(gbif_user|gbif_email|gbif_pwd)",
                            lines)
      lines <- lines[!to_overwrite]
    }
  }

  # Add/update lines
  update_lines <- paste0("gbif_user=", gbif_username, "\n",
                         "gbif_email=", gbif_email, "\n",
                         "gbif_pwd=", gbif_password)
  new_lines <- c(lines,
                 paste0("gbif_user=", "'", gbif_username, "'"),
                 paste0("gbif_email=", "'", gbif_email, "'"),
                 paste0("gbif_pwd=", "'", gbif_password, "'"))

  # Write lines
  writeLines(new_lines, renviron_path)

  # Reload the .Renviron file
  readRenviron(renviron_path)

  message("GBIF credentials have been processed and added/updated in your .Renviron file\n",
"Check your .Renviron with file.edit('", normalizePath(renviron_path, winslash = "/"), "')")

  if(open_Renviron){
    file.edit(normalizePath(renviron_path, winslash = "/"))
  }
}
