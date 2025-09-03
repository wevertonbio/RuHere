rspecieslink2 <- function (key = NULL, dir = "results/", filename = "output",
                          save = FALSE, basisOfRecord = NULL, family = NULL,
                          species = NULL, collectionCode = NULL, country = NULL,
                          stateProvince = NULL, county = NULL,
                          Coordinates = NULL, CoordinatesQuality = NULL,
                          Scope = NULL, Synonyms = "no synomyms", Typus = FALSE,
                          Images = NULL, RedList = FALSE, limit = NULL,
                          file.format = "csv", compress = FALSE){
  my_url <- "https://specieslink.net/ws/1.0/search?"
  url_query <- function(vector, name) {
    char <- paste(paste0(vector, "&"), collapse = "")
    url <- paste0(name, "=", char)
    return(url)
  }
  if (is.null(key)) {
    stop("Please provide an API key! Get one at specieslink.net/aut/profile/apikeys")
  }
  if (is.null(basisOfRecord)) {
    my_url
  }
  else {
    if (basisOfRecord %in% c("PreservedSpecimen", "LivingSpecimen",
                             "FossilSpecimen", "HumanObservation", "MachineObservation",
                             "MaterialSample")) {
      br <- url_query(basisOfRecord, "basisOfRecord")
      my_url <- paste0(my_url, br)
    }
  }
  if (is.null(species)) {
    my_url
  } else {
    if (is.character(species)) {
      if (length(species) > 50)
        stop("Please make request of no more than 50 species at a time!")
      species <- gsub(" ", "+", species)
      sp <- url_query(species, "scientificName")
      my_url <- paste0(my_url, sp)
    }
    else {
      stop("species must be a character")
    }
  }
  if (is.null(family)) {
    my_url
  } else {
    if (is.character(family)) {
      fam <- url_query(family, "family")
      my_url <- paste0(my_url, fam)
    }
    else {
      stop("family name must be a character")
    }
  }
  if (is.null(collectionCode)) {
    my_url
  }  else {
    if (is.character(collectionCode)) {
      cc <- url_query(collectionCode, "collectionCode")
      my_url <- paste0(my_url, cc)
    }
  }
  if (is.null(country)) {
    my_url
  }  else {
    if (is.character(country)) {
      country <- gsub(" ", "+", country)
      ct <- url_query(country, "country")
      my_url <- paste0(my_url, ct)
    }
  }
  if (is.null(stateProvince)) {
    my_url
  } else {
    if (is.character(stateProvince)) {
      stateProvince <- gsub(" ", "+", stateProvince)
      st <- url_query(stateProvince, "stateProvince")
      my_url <- paste0(my_url, st)
    }
  }
  if (is.null(county)) {
    my_url
  }  else {
    if (is.character(county)) {
      county <- gsub(" ", "+", county)
      co <- url_query(county, "county")
      my_url <- paste0(my_url, co)
    }
  }
  if (is.null(Coordinates)) {
    my_url
  } else {
    if (Coordinates %in% c("Yes", "No", "Original", "Automatic",
                           "Blocked")) {
      xy <- url_query(Coordinates, "Coordinates")
      my_url <- paste0(my_url, xy)
    }
  }
  if (is.null(CoordinatesQuality)) {
    my_url
  }   else {
    if (CoordinatesQuality %in% c("Good", "Bad")) {
      cq <- url_query(CoordinatesQuality, "CoordinatesQuality")
      my_url <- paste0(my_url, cq)
    }
  }
  if (is.null(Scope)) {
    my_url
  }   else {
    if (Scope %in% c("p", "a", "m", "f", "b")) {
      sc <- url_query(Scope, "Scope")
      my_url <- paste0(my_url, sc)
    }
  }
  if (is.null(Synonyms)) {
    my_url
  }  else {
    if (Synonyms %in% c("species2000", "flora2020", "MycoBank",
                        "AlgaeBase", "DSMZ")) {
      sy <- url_query(Synonyms, "Synonyms")
      my_url <- paste0(my_url, sy)
    }
  }
  if (Typus == FALSE) {
    my_url
  }  else {
    my_url <- paste0(my_url, "Typus/Yes/")
  }
  if (is.null(Images)) {
    my_url
  }   else {
    if (Images %in% c("Yes", "Live", "Polen", "Wood")) {
      im <- url_query(Images, "Images")
      my_url <- paste0(my_url, im)
    }
  }
  if (RedList == FALSE) {
    my_url
  }  else {
    my_url <- paste0(my_url, "RedList/Yes/")
  }
  if (is.null(limit)) {
    my_url
  }  else {
    if (is.numeric(limit)) {
      mr <- url_query(limit, "limit")
      my_url <- paste0(my_url, mr)
    }
  }
  my_url <- paste0(my_url, "apikey=", key)
  message("Making request to speciesLink...")
  df <- jsonlite::fromJSON(my_url)$features$properties
  if (save) {
    if (!dir.exists(dir)) {
      dir.create(dir)
    }
    if (file.format == "csv") {
      if (compress) {
        fullname <- paste0(dir, filename, ".csv.zip")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname, compress = "gzip")
      }
      else {
        fullname <- paste0(dir, filename, ".csv")
        message(paste0("Writing ", fullname, " on disk."))
        data.table::fwrite(df, file = fullname)
      }
    }
    if (file.format == "rds") {
      fullname <- paste0(dir, filename, ".rds")
      message(paste0("Writing ", fullname, " on disk."))
      if (compress) {
        saveRDS(df, file = fullname, compress = "gzip")
      }
      else {
        saveRDS(df, file = fullname, compress = FALSE)
      }
    }
  }
  if (is.null(dim(df))) {
    warning("Output is empty. Check your request.")
  }
  warning("Please make sure that the restrictions and citation indicated by\n  each speciesLink/CRIA data provider are observed and respected.")
  return(df)
}
