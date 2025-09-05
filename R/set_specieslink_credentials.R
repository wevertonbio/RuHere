set_specieslink_credentials <- function(specieslink_key, overwrite = FALSE,
                                        open_Renviron = FALSE) {
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

# # # Test
# specieslink_key = "teste"
# set_specieslink_credentials(specieslink_key = specieslink_key, overwrite = TRUE,
#                             open_Renviron = TRUE)


