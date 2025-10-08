get_specieslink <- function (key = NULL, dir = "results/", filename = "output",
                            save = FALSE, basisOfRecord = NULL, family = NULL,
                            species = NULL, institutionCode = NULL,
                            collectionID = NULL, catalogNumber = NULL,
                            kingdom = NULL, phylum = NULL, class = NULL,
                            order = NULL, genus = NULL, specificEpithet = NULL,
                            infraspecificEpithet = NULL, collectionCode = NULL,
                            identifiedBy = NULL, yearIdentified = NULL,
                            country = NULL, stateProvince = NULL, county = NULL,
                            typeStatus = NULL, recordedBy = NULL,
                            recordNumber = NULL, yearCollected = NULL,
                            locality = NULL, occurrenceRemarks = NULL,
                            barcode = NULL, bbox = NULL, landuse_1 = NULL,
                            landuse_year_1 = NULL, landuse_2 = NULL,
                            landuse_year_2 = NULL, phonetic = FALSE,
                            coordinates = NULL, scope = NULL, synonyms = NULL,
                            typus = FALSE, images = FALSE, redlist = NULL,
                            limit = NULL, file.format = "csv",
                            compress = FALSE) {

  base_url <- "https://specieslink.net/ws/1.0/search?"

  url_query <- function(vector, name) {
    char <- paste(paste0(vector, "&"), collapse = "")
    url <- paste0(name, "=", char)
    return(url)
  }

  # Get key from Renvironment
  if (is.null(key)) {
    key <- Sys.getenv("specieslink_key")
  }

  # Check if key exists
  if (key == "") {
    stop(
      "API key not found.\n",
      "Please save your API key to your R environment using:\n",
      "  set_specieslink_credentials(\"your_api_key\")\n\n",
      "You can find your API key at:\n",
      "https://specieslink.net/aut/profile/apikeys"
    )
  }

  # basisOfRecord
  if (is.null(basisOfRecord)) {
    base_url
  } else {
    if (is.character(basisOfRecord)) {
      if (basisOfRecord %in% c("PreservedSpecimen", "LivingSpecimen",
                               "FossilSpecimen", "HumanObservation",
                               "MachineObservation", "MaterialSample")) {
        bor <- url_query(basisOfRecord, "basisOfRecord")
        base_url <- paste0(base_url, bor)
      }
    } else {
        stop("basisOfRecord must be a character: PreservedSpecimen, LivingSpecimen, FossilSpecimen, HumanObservation, MachineObservation, MaterialSample")
    }
  }

  # family
  if (is.null(family)) {
    base_url
  } else {
    if (is.character(family)) {
      fam <- url_query(family, "family")
      base_url <- paste0(base_url, fam)
    } else {
      stop("family name must be a character")
    }
  }

  # species
  if (is.null(species)) {
    base_url
  } else {
    if (is.character(species)) {
      if (length(species) > 50)
        stop("Please make request of no more than 50 species at a time!")
      species <- gsub(" ", "+", species)
      spp <- url_query(species, "scientificName")
      base_url <- paste0(base_url, spp)
    } else {
      stop("species must be a character")
    }
  }

  # institutionCode
  if (is.null(institutionCode)) {
    base_url
  } else {
    if (is.character(institutionCode)) {
      inc <- url_query(institutionCode, "institutionCode")
      base_url <- paste0(base_url, inc)
    } else {
      stop("institutionCode must be a character")
    }
  }

  # collectionID
  if (is.null(collectionID)) {
    base_url
  } else {
    if (is.character(collectionID)) {
      cid <- url_query(collectionID, "collectionID")
      base_url <- paste0(base_url, cid)
    } else {
      stop("collectionID must be a character")
    }
  }

  # catalogNumber
  if (is.null(catalogNumber)) {
    base_url
  } else {
    if (is.character(catalogNumber)) {
      cnb <- url_query(catalogNumber, "catalogNumber")
      base_url <- paste0(base_url, cnb)
    } else {
      stop("catalogNumber must be a character")
    }
  }

  # kingdom
  if (is.null(kingdom)) {
    base_url
  } else {
    if (is.character(kingdom)) {
      kin <- url_query(kingdom, "kingdom")
      base_url <- paste0(base_url, kin)
    } else {
      stop("kingdom must be a character")
    }
  }

  # phylum
  if (is.null(phylum)) {
    base_url
  } else {
    if (is.character(phylum)) {
      phy <- url_query(phylum, "phylum")
      base_url <- paste0(base_url, phy)
    } else {
      stop("phylum must be a character")
    }
  }

  # class
  if (is.null(class)) {
    base_url
  } else {
    if (is.character(class)) {
      cla <- url_query(class, "class")
      base_url <- paste0(base_url, cla)
    } else {
      stop("class must be a character")
    }
  }

  # order
  if (is.null(order)) {
    base_url
  } else {
    if (is.character(order)) {
      ord <- url_query(order, "order")
      base_url <- paste0(base_url, ord)
    } else {
      stop("order must be a character")
    }
  }

  # genus
  if (is.null(genus)) {
    base_url
  } else {
    if (is.character(genus)) {
      gen <- url_query(genus, "genus")
      base_url <- paste0(base_url, gen)
    } else {
      stop("genus must be a character")
    }
  }

  # specificEpithet
  if (is.null(specificEpithet)) {
    base_url
  } else {
    if (is.character(specificEpithet)) {
      spe <- url_query(specificEpithet, "specificEpithet")
      base_url <- paste0(base_url, spe)
    } else {
      stop("specificEpithet must be a character")
    }
  }

  # infraspecificEpithet
  if (is.null(infraspecificEpithet)) {
    base_url
  } else {
    if (is.character(infraspecificEpithet)) {
      ise <- url_query(infraspecificEpithet, "infraspecificEpithet")
      base_url <- paste0(base_url, ise)
    } else {
      stop("infraspecificEpithet must be a character")
    }
  }

  # colletcionCode
  if (is.null(collectionCode)) {
    base_url
  }  else {
    if (is.character(collectionCode)) {
      cc <- url_query(collectionCode, "collectionCode")
      base_url <- paste0(base_url, cc)
    } else {
      stop("collectionCode must be a character")
    }
  }

  # identifiedBy
  if (is.null(identifiedBy)) {
    base_url
  } else {
    if (is.character(identifiedBy)) {
      identifiedBy <- gsub(" ", "+", identifiedBy)
      iby <- url_query(identifiedBy, "identifiedBy")
      base_url <- paste0(base_url, iby)
    } else {
      stop("identifiedBy must be a character")
    }
  }

  # yearIdentified
  if (is.null(yearIdentified)) {
    base_url
  } else {
    if (is.numeric(yearIdentified)) {
      if (yearIdentified >= 1000 && yearIdentified <= 9999) {
        yea <- url_query(as.character(yearIdentified), "yearIdentified")
        base_url <- paste0(base_url, yea)
      } else {
        stop("yearIdentified must be a four-digit year (1000-9999)")
      }
    } else {
      stop("yearIdentified must be a numeric value")
    }
  }

  # country
  if (is.null(country)) {
    base_url
  }  else {
    if (is.character(country)) {
      country <- gsub(" ", "+", country)
      ct <- url_query(country, "country")
      base_url <- paste0(base_url, ct)
    } else {
      stop("country must be a character")
    }
  }

  # stateProvince
  if (is.null(stateProvince)) {
    base_url
  } else {
    if (is.character(stateProvince)) {
      stateProvince <- gsub(" ", "+", stateProvince)
      st <- url_query(stateProvince, "stateProvince")
      base_url <- paste0(base_url, st)
    } else {
      stop("stateProvince must be a character")
    }
  }

  # county
  if (is.null(county)) {
    base_url
  }  else {
    if (is.character(county)) {
      county <- gsub(" ", "+", county)
      co <- url_query(county, "county")
      base_url <- paste0(base_url, co)
    } else {
      stop("county must be a character")
    }
  }

  # typeStatus
  if (is.null(typeStatus)) {
    base_url
  } else {
    if (is.character(typeStatus)) {
        typ <- url_query(typeStatus, "typeStatus")
        base_url <- paste0(base_url, typ)
    } else {
      stop("typeStatus must be a character")
    }
  }

  # recordedBy
  if (is.null(recordedBy)) {
    base_url
  } else {
    if (is.character(recordedBy)) {
      recordedBy <- gsub(" ", "+", recordedBy)
      rby <- url_query(recordedBy, "recordedBy")
      base_url <- paste0(base_url, rby)
    } else {
      stop("recordedBy must be a character")
    }
  }

  # recordNumber
  if (is.null(recordNumber)) {
    base_url
  } else {
    if (is.numeric(recordNumber)) {
      rn <- url_query(as.character(recordNumber), "recordNumber")
      base_url <- paste0(base_url, rn)
    } else {
      stop("recordNumber must be numeric")
    }
  }

  # yearCollected
  if (is.null(yearCollected)) {
    base_url
  } else {
    if (is.numeric(yearCollected)) {
      if (yearCollected >= 1000 && yearCollected <= 9999) {
        yec <- url_query(as.character(yearCollected), "yearCollected")
        base_url <- paste0(base_url, yec)
      } else {
        stop("yearCollected must be a four-digit year (1000-9999)")
      }
    } else {
      stop("yearCollected must be a numeric value")
    }
  }

  # locality
  if (is.null(locality)) {
    base_url
  } else {
    if (is.character(locality)) {
      locality <- gsub(" ", "+", locality)
      loc <- url_query(locality, "locality")
      base_url <- paste0(base_url, loc)
    } else {
      stop("locality must be a character")
    }
  }

  # coordinates
  if (is.null(coordinates)) {
    base_url
  } else {
    if (is.character(coordinates)) {
      if (coordinates %in% c("yes", "no", "original", "automatic",
                             "blocked", "consistent", "suspect")) {
        xy <- url_query(coordinates, "coordinates")
        base_url <- paste0(base_url, xy)
      } else {
        stop("coordinates must be a character: yes, no, original, automatic, blocked, consistent, suspect")
      }
    }
  }

  # occurrenceRemarks
  if (is.null(occurrenceRemarks)) {
    base_url
  } else {
    if (is.character(occurrenceRemarks)) {
      occurrenceRemarks <- gsub(" ", "+", occurrenceRemarks)
      or <- url_query(occurrenceRemarks, "occurrenceRemarks")
      base_url <- paste0(base_url, or)
    } else {
      stop("occurrenceRemarks must be a character")
    }
  }

  # barcode
  if (is.null(barcode)) {
    base_url
  } else {
    if (is.character(barcode)) {
      bc <- url_query(barcode, "barcode")
      base_url <- paste0(base_url, bc)
    } else {
      stop("barcode must be a character")
    }
  }

  #if (is.null(CoordinatesQuality)) {
  #  base_url
  #} else {
  #  if (is.character(CoordinatesQuality)) {
  #    if (CoordinatesQuality %in% c("Good", "Bad")) {
  #      cq <- url_query(CoordinatesQuality, "CoordinatesQuality")
  #      base_url <- paste0(base_url, cq)
  #    }
  #  } else {
  #    stop("CoordinatesQuality must be a character")
  #  }
  #}

  # bbox
  if (is.null(bbox)) {
  base_url
  } else {
    if (is.character(bbox)) {
      bbox_parts <- strsplit(bbox, "[+]")[[1]]
      if (length(bbox_parts) == 4) {
        coords <- suppressWarnings(as.numeric(bbox_parts))
        if (all(!is.na(coords))) {
          long <- coords[c(1, 3)]
          lat <- coords[c(2, 4)]
          if (all(long >= -180 & long <= 180) && all(lat >= -90 & lat <= 90)) {
              if (coords[1] < coords[3] && coords[2] < coords[4]) {
                bbo <- url_query(bbox, "bbox")
                base_url <- paste0(base_url, bbo)
            } else {
              stop("bbox must represent a valid rectangle: longitude1 < longitude2 and latitude1 < latitude2")
            }
          } else {
            stop("bbox coordinates must be within valid ranges: longitude (-180 to 180), latitude (-90 to 90)")
          }
        } else {
          stop("bbox must contain only numeric coordinates separated by '+'")
        }
      } else {
        stop("bbox must contain exactly 4 coordinates in format: 'longitude1+latitude1+longitude2+latitude2'")
      }
    } else {
      stop("bbox must be a character string")
    }
  }

  # landuse_1
  if (is.null(landuse_1)) {
    base_url
  } else {
    if (is.character(landuse_1)) {
      landuse_codes <- strsplit(landuse_1, ",")[[1]]
      if (all(grepl("^[0-9]+$", landuse_codes))) {
        lu1 <- url_query(landuse_1, "landuse_1")
        base_url <- paste0(base_url, lu1)
      } else {
        stop("landuse_1 must contain comma-separated numeric codes (e.g., '23,33')")
      }
    } else {
      stop("landuse_1 must be a character string")
    }
  }

  # landuse_year_1
  if (is.null(landuse_year_1)) {
    base_url
  } else {
    if (is.numeric(landuse_year_1)) {
      if (landuse_year_1 >= 1985 && landuse_year_1 <= 2021) {
        ly1 <- url_query(as.character(landuse_year_1), "landuse_year_1")
        base_url <- paste0(base_url, ly1)
      } else {
        stop("landuse_year_1 must be a year between 1985 and 2021")
      }
    } else {
      stop("landuse_year_1 must be a numeric value")
    }
  }

  # landuse_2
  if (is.null(landuse_2)) {
    base_url
  } else {
    if (is.character(landuse_2)) {
      landuse_codes <- strsplit(landuse_2, ",")[[1]]
      if (all(grepl("^[0-9]+$", landuse_codes))) {
        lu2 <- url_query(landuse_2, "landuse_2")
        base_url <- paste0(base_url, lu2)
      } else {
        stop("landuse_2 must contain comma-separated numeric codes (e.g., '15,9,21')")
      }
    } else {
      stop("landuse_2 must be a character string")
    }
  }

  # landuse_year_2
  if (is.null(landuse_year_2)) {
    base_url
  } else {
    if (is.numeric(landuse_year_2)) {
      if (landuse_year_2 >= 1985 && landuse_year_2 <= 2021) {
        ly2 <- url_query(as.character(landuse_year_2), "landuse_year_2")
        base_url <- paste0(base_url, ly2)
      } else {
        stop("landuse_year_2 must be a year between 1985 and 2021")
      }
    } else {
      stop("landuse_year_2 must be a numeric value")
    }
  }

  # phonetic
  if (phonetic == FALSE) {
    base_url
  } else {
    if (is.logical(phonetic)) {
      base_url <- paste0(base_url, "phonetic/yes/")
    } else {
      stop("phonetic must be TRUE or FALSE")
    }
  }

  if (is.null(scope)) {
    base_url
  } else {
    if (is.character(scope)) {
      if (scope %in% c("p", "a", "m", "f", "b")) {
        sc <- url_query(scope, "scope")
        base_url <- paste0(base_url, sc)
      }
    } else {
      stop("scope must be a character: a (animals), p (plants), b (broad scope), m (microorganisms), f (fossils)")
    }
  }

  if (is.null(synonyms)) {
    base_url
  } else {
    if (is.character(synonyms)) {
      if (synonyms %in% c("sp2000", "flora2020", "MycoBank",
                          "algaebase", "DSMZ", "moure")) {
        sy <- url_query(synonyms, "synonyms")
        base_url <- paste0(base_url, sy)
      }
    } else {
      stop("synonyms must be a character: sp2000 (Species 2000), flora2020 (Flora e Funga do Brasil), MycoBank, algaebase, DSMZ, moure (Catálogo Moure)")
    }
  }

  if (typus == FALSE) {
    base_url
  } else {
    if(is.logical(typus)) {
      base_url <- paste0(base_url, "typus/yes/")
    } else {
      stop("typus must be TRUE or FALSE")
    }
  }

  if (images == FALSE) {
    base_url
  } else {
    if(is.logical(images)) {
      base_url <- paste0(base_url, "images/yes/")
    } else {
      stop("images must be TRUE or FALSE")
    }
  }

  if (is.null(redlist)) {
    base_url
  } else {
    if (is.character(redlist)) {
      if (redlist %in% c("CR", "PEX", "EN", "EW", "EX", "RE")) {
        rl <- url_query(redlist, "redlist")
        base_url <- paste0(base_url, rl)
      }
    } else {
      stop("redlist must be a character: CR (critically endangered), PEX (Probably extinct), EN (endangered), EW (Extinct in nature), EX (Extinct), RE (Extinct in Brazil)")
    }
  }

  if (!is.null(limit)) {
    if (is.numeric(limit)) {
      mr <- url_query(limit, "limit")
      base_url <- paste0(base_url, mr)
    } else {
      stop("limit must be numeric")
    }
  }

  base_url <- paste0(base_url, "apikey=", key)

  message("Making request to speciesLink...")

  df_json <- jsonlite::fromJSON(base_url)

  n_records <- df_json$numberMatched

  if (n_records > 5000) {
    n_requests <- 1:ceiling(n_records/5000) - 1
    
    list_urls <- lapply(n_requests, function(x) {
      offset_x <- ifelse(x == 0, 0, (x * 5000))
      paste0(base_url, "&limit=5000", "&offset=", x * offset_x)
    })

    df_lim <- lapply(list_urls, 
                     function(x) jsonlite::fromJSON(x)$features$properties)
    
    df <- dplyr::bind_rows(df_lim)
  } else {
    df <- df_json$features$properties
  }

  if (!is.logical(save)) {
    stop("save must be TRUE or FALSE")
  }

  if (save) {

    if (!file.format %in% c("csv", "rds")) {
      stop("file.format must be either 'csv' or 'rds'")
    }

    if (!dir.exists(dir)) {
      dir.create(dir)
    }

    if (!is.logical(compress)) {
      stop("compress must be TRUE or FALSE")
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

# # # Test
# specieslink_key = "teste"
# set_specieslink_credentials(specieslink_key = specieslink_key,
#                             overwrite = TRUE, open_Renviron = TRUE)
# res_test <- get_specieslink(
#     family = "Arecaceae",
#     country = "BR",
#     stateProvince = "São Paulo",
#     basisOfRecord = "PreservedSpecimen",
#     limit = 5,
#     save = FALSE
#   )
