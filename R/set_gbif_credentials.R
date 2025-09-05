set_gbif_credentials <- function(gbif_username, gbif_email, gbif_password,
                                 overwrite = FALSE, open_Renviron = FALSE) {
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

# # Test
# gbif_username = "wevertonf1993"
# gbif_email = "wevertonf1993@hotmail.com"
# gbif_password = "teste"
#
#

# set_gbif_credentials(gbif_username = gbif_username, gbif_email = gbif_email,
#                      gbif_password = gbif_password, overwrite = TRUE,
#                      open_Renviron = TRUE)


