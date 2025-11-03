set_iucn_credentials <- function(iucn_key,
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

