get_idigbio <- function(species = NULL, fields = "all",
                        genus = NULL, family = NULL,
                        order = NULL, phylum = NULL, kingdom = NULL,
                        country = NULL, county = NULL, limit = NULL,
                        offset = NULL, dir, filename = "idigbio_output",
                        save = FALSE, compress = FALSE, ...) {

  rq = list()

  if (!is.null(species) && !inherits(species, "character")) {
      stop("species must be a character")
  } else {
    rq$scientificname <- species
  }

  if (!is.null(genus) && !inherits(genus, "character")) {
      stop("genus must be a character")
  } else {
    rq$genus <- genus
  }

  if (!is.null(family) && !inherits(family, "character")) {
      stop("family must be a character")
  } else {
    rq$family <- family
  }

  if (!is.null(order) && !inherits(order, "character")) {
      stop("order must be a character")
  } else {
    rq$order <- order
  }

  if (!is.null(phylum) && !inherits(phylum, "character")) {
      stop("phylum must be a character")
  } else {
    rq$phylum <- phylum
  }

  if (!is.null(kingdom) && !inherits(kingdom, "character")) {
      stop("kingdom must be a character")
  } else {
    rq$kingdom <- kingdom
  }

  if (!is.null(country) && !inherits(country, "character")) {
      stop("country must be a character")
  } else {
    rq$country <- country
  }

  if (!is.null(county) && !inherits(county, "character")) {
      stop("county must be a character")
  } else {
    rq$county <- county
  }

  if (!is.null(limit)) {
    if(!inherits(limit, "numeric")) {
      stop("limit must be numeric")
    }
  } else {
    limit <- 0
  }

  if (!is.null(offset)) {
    if(!inherits(offset, "numeric")) {
      stop("offset must be numeric")
    }
  } else {
    offset <- 0
  }

  if (!inherits(save, "logical")) {
    stop("save must be logical")
  }

  df <- ridigbio::idig_search_records(rq=rq, fields = fields,
                                      limit = limit, offset = offset)

  df$scientificname <- florabr:::firstup(df$scientificname)

  colnames(df)[colnames(df) == "geopoint.lon"] <- "lon"
  colnames(df)[colnames(df) == "geopoint.lat"] <- "lat"
  colnames(df)[colnames(df) == "data.dwc:eventDate"] <- "eventDate"
  colnames(df)[colnames(df) == "data.dwc:year"] <- "year"
  colnames(df)[colnames(df) == "data.dwc:month"] <- "month"
  colnames(df)[colnames(df) == "data.dwc:day"] <- "day"

  if (save) {

    if (!inherits(dir, "character")) {
      stop("dir must be a character")
    }

    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }

    if (!inherits(filename, "character")) {
      stop("filename must be a character")
    }

    if (!inherits(compress, "logical")) {
      stop("compress must be logical")
    }

    if (compress) {
      fullname <- paste0(dir, filename, ".csv.zip")
      data.table::fwrite(df, file = fullname, compress = "gzip")
    } else {
      fullname <- paste0(dir, "/", filename, ".csv")
      data.table::fwrite(df, file = fullname)
    }

  }

  return(df)
}

# # # Test
# res_test <- get_idigbio(
#     species = c("Araucaria angustifolia", "Paubrasilia echinata")
# )
