#' @title get_specieslink

#' @usage get_specieslink(key = NULL, dir, filename = "output", save = FALSE,
#' basisOfRecord = NULL, family = NULL, species = NULL, institutionCode = NULL,
#' collectionID = NULL, catalogNumber = NULL, kingdom = NULL, phylum = NULL,
#' class = NULL, order = NULL, genus = NULL, specificEpithet = NULL,
#' infraspecificEpithet = NULL, collectionCode = NULL, identifiedBy = NULL,
#' yearIdentified = NULL, country = NULL, stateProvince = NULL, county = NULL,
#' typeStatus = NULL, recordedBy = NULL, recordNumber = NULL,
#' yearCollected = NULL, locality = NULL, occurrenceRemarks = NULL,
#' barcode = NULL, bbox = NULL, landuse_1 = NULL, landuse_year_1 = NULL,
#' landuse_2 = NULL, landuse_year_2 = NULL, phonetic = FALSE,
#' coordinates = NULL, scope = NULL, synonyms = NULL, typus = FALSE,
#' images = FALSE, redlist = NULL, limit = NULL, file.format = "csv",
#' compress = FALSE)
#'
#' @description
#' Retrieves occurrence data from the [speciesLink](https://specieslink.net/)
#' network using user-defined filters. The function allows querying by
#' taxonomic, geographic, and collection-related parameters.
#'
#' @param key (character) API key or authentication token if required.
#' @param dir (character) Directory where files will be saved (if `save = TRUE`).
#' @param filename (character) Name of the output file without extension. Default is `"output"`.
#' @param save (logical) Whether to save the results to file. Default is `FALSE`.
#' @param basisOfRecord (character) Filter by basis of record.
#' @param family (character) Family name.
#' @param species (character) Species name.
#' @param institutionCode (character) Code of the institution that holds the specimen.
#' @param collectionID (character) Unique identifier for the collection.
#' @param catalogNumber (character) Catalog number of the specimen or record.
#' @param kingdom (character) Kingdom name.
#' @param phylum (character) Phylum name.
#' @param class (character) Class name.
#' @param order (character) Order name.
#' @param genus (character) Genus name.
#' @param specificEpithet (character) Specific epithet of the species.
#' @param infraspecificEpithet (character) Infraspecific epithet.
#' @param collectionCode (character) Code identifying the collection within an institution.
#' @param identifiedBy (character) Name of the person who identified the specimen.
#' @param yearIdentified (numeric) Year of identification.
#' @param country (character) Country name.
#' @param stateProvince (character) State or province name.
#' @param county (character) County or municipality name.
#' @param typeStatus (character) Type status.
#' @param recordedBy (character) Collector name.
#' @param recordNumber (numeric) Collector’s record number.
#' @param yearCollected (numeric) Year of collection.
#' @param locality (character) Locality description.
#' @param occurrenceRemarks (character) Text field for remarks about the occurrence.
#' @param barcode (character) Barcode or unique specimen identifier.
#' @param bbox (character) Bounding box coordinates in the format `"lon_min+lat_min+lon_max+lat_max"`.
#' @param landuse_1 (character) Land use category for the first year.
#' @param landuse_year_1 (numeric) Year corresponding to `landuse_1`.
#' @param landuse_2 (character) Land use category for the second year.
#' @param landuse_year_2 (numeric) Year corresponding to `landuse_2`.
#' @param phonetic (logical) Whether to use phonetic matching for taxon names. Default is `FALSE`.
#' @param coordinates (character) Whether to include only records with geographic coordinates (`"yes"`, `"no"`, `"original"`, `"automatic"`, `"blocked"`, `"consistent"`, `"suspect"`)). Default is `NULL`.
#' @param scope (character) Scope of the query (`"p"`, `"a"`, `"m"`, `"f"`, `"b"`). Default is `NULL`.
#' @param synonyms (chacarter) Whether to include synonyms of the specified taxon (`"sp2000"`, `"flora2020"`, `"MycoBank"`, `"algaebase"`, `"DSMZ"`, `"moure"`). Default is `NULL`.
#' @param typus (logical) Whether to filter only type specimens. Default is `FALSE`.
#' @param images (logical) Whether to restrict to records with associated images. Default is `FALSE`.
#' @param redlist (character) Filter by IUCN Red List category.
#' @param limit (numeric) Maximum number of records to return.
#' @param file.format (character) File format for saving output (`"csv"`, `"rds"`). Default is `"csv"`.
#' @param compress (logical) Whether to compress the output file into `.zip`. Default is `FALSE`.
#'
#'#' @details
#' The speciesLink API key can be set permanently using:
#' \preformatted{
#' set_specieslink_credentials("your_api_key")
#' }
#'
#' @return A \code{data.frame} containing the occurrence data fields returned by speciesLink.
#'
#' @importFrom dplyr bind_rows
#' @importFrom jsonlite fromJSON
#' @importFrom data.table fwrite
#'
#' @examples
#' \dontrun{
#' # Set your API key once per session
#' set_specieslink_credentials("your_api_key")
#'
#' # Retrieve records for Arecaceae in São Paulo
#' res <- get_specieslink(
#'   family = "Arecaceae",
#'   country = "Brazil",
#'   stateProvince = "São Paulo",
#'   basisOfRecord = "PreservedSpecimen",
#'   limit = 10
#' )
#'
#' # Save results as compressed CSV
#' get_specieslink(
#'   family = "Arecaceae",
#'   country = "Brazil",
#'   save = TRUE,
#'   dir = "data/",
#'   filename = "arecaceae_sp",
#'   compress = TRUE
#' )
#' }
#'
#' @export
get_specieslink <- function (key = NULL, dir, filename = "output",
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

  if (!missing(save) && isTRUE(save)) {
    if (missing(dir) || is.null(dir) || !inherits(dir, "character") || length(dir) != 1) {
      stop("'dir' must be a single character string when save = TRUE, not ",
        paste(class(dir), collapse = "/"))
    }
  } else {
    if (!missing(dir) && !is.null(dir) && !inherits(dir, "character")) {
      stop("'dir' must be a character if provided, not ", paste(class(dir), collapse = "/"))
    }
  }

  if (!inherits(filename, "character") || length(filename) != 1)
    stop("'filename' must be a single character value, not ", paste(class(filename), collapse = "/"))

  if (!is.null(key) && (!inherits(key, "character") || length(key) != 1))
    stop("'key' must be a single character string if provided, not ", paste(class(key), collapse = "/"))

  if (!inherits(file.format, "character") || length(file.format) != 1)
    stop("'file.format' must be a single character ('csv' or 'rds'), not ", paste(class(file.format), collapse = "/"))

  if (!file.format %in% c("csv", "rds"))
    stop("'file.format' must be either 'csv' or 'rds'")

  if (!inherits(save, "logical") || length(save) != 1)
    stop("'save' must be a single logical (TRUE/FALSE), not ", paste(class(save), collapse = "/"))

  if (!inherits(phonetic, "logical") || length(phonetic) != 1)
    stop("'phonetic' must be a single logical (TRUE/FALSE), not ", paste(class(phonetic), collapse = "/"))

  if (!inherits(typus, "logical") || length(typus) != 1)
    stop("'typus' must be a single logical (TRUE/FALSE), not ", paste(class(typus), collapse = "/"))

  if (!inherits(images, "logical") || length(images) != 1)
    stop("'images' must be a single logical (TRUE/FALSE), not ", paste(class(images), collapse = "/"))

  if (!inherits(compress, "logical") || length(compress) != 1)
    stop("'compress' must be a single logical (TRUE/FALSE), not ", paste(class(compress), collapse = "/"))

  if (!is.null(limit) && !inherits(limit, "numeric"))
    stop("'limit' must be numeric, not ", paste(class(limit), collapse = "/"))

  if (!is.null(recordNumber) && !inherits(recordNumber, "numeric"))
    stop("'recordNumber' must be numeric, not ", paste(class(recordNumber), collapse = "/"))

  if (!is.null(yearIdentified)) {
    if (!inherits(yearIdentified, "numeric")) stop("'yearIdentified' must be numeric (four-digit year), not ", paste(class(yearIdentified), collapse = "/"))
    if (any(yearIdentified < 1000 | yearIdentified > 9999)) stop("'yearIdentified' must be a four-digit year between 1000 and 9999")
  }

  if (!is.null(yearCollected)) {
    if (!inherits(yearCollected, "numeric")) stop("'yearCollected' must be numeric (four-digit year), not ", paste(class(yearCollected), collapse = "/"))
    if (any(yearCollected < 1000 | yearCollected > 9999)) stop("'yearCollected' must be a four-digit year between 1000 and 9999")
  }

  if (!is.null(landuse_year_1) && (!inherits(landuse_year_1, "numeric") || any(landuse_year_1 < 1985 | landuse_year_1 > 2021)))
    stop("'landuse_year_1' must be numeric year between 1985 and 2021")

  if (!is.null(landuse_year_2) && (!inherits(landuse_year_2, "numeric") || any(landuse_year_2 < 1985 | landuse_year_2 > 2021)))
    stop("'landuse_year_2' must be numeric year between 1985 and 2021")

  if (!is.null(species) && length(species) > 50)
    stop("Please request no more than 50 species at a time (argument 'species')")

  if (!is.null(bbox)) {
    if (!inherits(bbox, "character") || length(bbox) != 1) {
      stop("'bbox' must be a single character in the format 'lon_min+lat_min+lon_max+lat_max'")
    }
    parts <- strsplit(bbox, "[+]")[[1]]
    if (length(parts) != 4) stop("'bbox' must contain exactly 4 values separated by '+'")
    coords <- suppressWarnings(as.numeric(parts))
    if (any(is.na(coords))) stop("'bbox' must contain only numeric coordinates separated by '+'")
    if (coords[1] < -180 || coords[1] > 180 || coords[3] < -180 || coords[3] > 180 ||
        coords[2] < -90 || coords[2] > 90 || coords[4] < -90 || coords[4] > 90) {
      stop("'bbox' coordinates must be within longitude (-180,180) and latitude (-90,90)")
    }
    if (!(coords[1] < coords[3] && coords[2] < coords[4])) {
      stop("'bbox' must represent a valid rectangle: lon_min < lon_max and lat_min < lat_max")
    }
  }

  if (!is.null(landuse_1)) {
    if (!inherits(landuse_1, "character")) stop("'landuse_1' must be a character string with comma-separated numeric codes")
    codes <- strsplit(landuse_1, ",")[[1]]
    if (!all(grepl("^[0-9]+$", codes))) stop("'landuse_1' must contain only numeric codes separated by commas (e.g. '23,33')")
  }
  if (!is.null(landuse_2)) {
    if (!inherits(landuse_2, "character")) stop("'landuse_2' must be a character string with comma-separated numeric codes")
    codes <- strsplit(landuse_2, ",")[[1]]
    if (!all(grepl("^[0-9]+$", codes))) stop("'landuse_2' must contain only numeric codes separated by commas (e.g. '15,9,21')")
  }

  if (!is.null(coordinates) && !inherits(coordinates, "character") && !all(coordinates %in% c("yes", "no", "original", "automatic", "blocked", "consistent", "suspect")))
    stop("'coordinates' contains invalid value(s). Allowed: 'yes','no','original','automatic','blocked','consistent','suspect'")

  if (!is.null(scope) && !inherits(scope, "character") && !all(scope %in% c("p", "a", "m", "f", "b")))
    stop("'scope' contains invalid value(s). Allowed: 'p' (plants), 'a' (animals), 'm' (microorganisms), 'f' (fossils), 'b' (broad)")

  if (!is.null(synonyms) && !inherits(synonyms, "character") && !all(synonyms %in% c("sp2000", "flora2020", "MycoBank", "algaebase", "DSMZ", "moure")))
    stop("'synonyms' contains invalid value(s). Allowed: 'sp2000','flora2020','MycoBank','algaebase','DSMZ','moure'")

  if (!is.null(redlist) && !inherits(redlist, "character") && !all(redlist %in% c("CR", "PEX", "EN", "EW", "EX", "RE")))
    stop("'redlist' contains invalid value(s). Allowed: 'CR','PEX','EN','EW','EX','RE'")

  text_args <- c("family", "institutionCode", "collectionID", "catalogNumber", "kingdom",
                 "phylum", "class", "order", "genus", "specificEpithet", "infraspecificEpithet",
                 "collectionCode", "identifiedBy", "country", "stateProvince", "county",
                 "typeStatus", "recordedBy", "locality", "occurrenceRemarks", "barcode")
  for (nm in text_args) {
    val <- get(nm)
    if (!is.null(val) && !inherits(val, "character")) {
      stop("'", nm, "' must be a character (or NULL), not ", paste(class(val), collapse = "/"))
    }
  }

  if (!is.null(filename) && grepl("[/\\\\]$", filename)) {
    warning("'filename' ends with a path separator — remove '/' or '\\\\' from filename (use dir for folder)")
  }

  base_url <- "https://specieslink.net/ws/1.0/search?"

  url_query <- function(vector, name) {
    if(length(vector)>1){
      vector <- paste(vector, collapse = ",")
    }
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
  if (!is.null(basisOfRecord)) {
    bor <- url_query(basisOfRecord, "basisOfRecord")
    base_url <- paste0(base_url, bor)
  }

  # family
  if (!is.null(family)) {
    fam <- url_query(family, "family")
    base_url <- paste0(base_url, fam)
  }

  # species
  if (!is.null(species)) {
    species <- gsub(" ", "+", species)
    spp <- url_query(vector = species, name = "scientificName")
    base_url <- paste0(base_url, spp)
  }

  # institutionCode
  if (!is.null(institutionCode)) {
    inc <- url_query(institutionCode, "institutionCode")
    base_url <- paste0(base_url, inc)
  }

  # collectionID
  if (!is.null(collectionID)) {
    cid <- url_query(collectionID, "collectionID")
    base_url <- paste0(base_url, cid)
  }

  # catalogNumber
  if (!is.null(catalogNumber)) {
    cnb <- url_query(catalogNumber, "catalogNumber")
    base_url <- paste0(base_url, cnb)
  }

  # kingdom
  if (!is.null(kingdom)) {
    kin <- url_query(kingdom, "kingdom")
    base_url <- paste0(base_url, kin)
  }

  # phylum
  if (!is.null(phylum)) {
    phy <- url_query(phylum, "phylum")
    base_url <- paste0(base_url, phy)
  }

  # class
  if (!is.null(class)) {
    cla <- url_query(class, "class")
    base_url <- paste0(base_url, cla)
  }

  # order
  if (!is.null(order)) {
    ord <- url_query(order, "order")
    base_url <- paste0(base_url, ord)
  }

  # genus
  if (!is.null(genus)) {
    gen <- url_query(genus, "genus")
    base_url <- paste0(base_url, gen)
  }

  # specificEpithet
  if (!is.null(specificEpithet)) {
    spe <- url_query(specificEpithet, "specificEpithet")
    base_url <- paste0(base_url, spe)
  }

  # infraspecificEpithet
  if (!is.null(infraspecificEpithet)) {
    ise <- url_query(infraspecificEpithet, "infraspecificEpithet")
    base_url <- paste0(base_url, ise)
  }

  # colletcionCode
  if (!is.null(collectionCode)) {
    cc <- url_query(collectionCode, "collectionCode")
    base_url <- paste0(base_url, cc)
  }

  # identifiedBy
  if (!is.null(identifiedBy)) {
    identifiedBy <- gsub(" ", "+", identifiedBy)
    iby <- url_query(identifiedBy, "identifiedBy")
    base_url <- paste0(base_url, iby)
  }

  # yearIdentified
  if (!is.null(yearIdentified)) {
    yea <- url_query(as.character(yearIdentified), "yearIdentified")
    base_url <- paste0(base_url, yea)
  }

  # country
  if (!is.null(country)) {
    country <- gsub(" ", "+", country)
    ct <- url_query(country, "country")
    base_url <- paste0(base_url, ct)
  }

  # stateProvince
  if (!is.null(stateProvince)) {
    stateProvince <- gsub(" ", "+", stateProvince)
    st <- url_query(stateProvince, "stateProvince")
    base_url <- paste0(base_url, st)
  }

  # county
  if (!is.null(county)) {
    county <- gsub(" ", "+", county)
    co <- url_query(county, "county")
    base_url <- paste0(base_url, co)
  }

  # typeStatus
  if (!is.null(typeStatus)) {
      typ <- url_query(typeStatus, "typeStatus")
      base_url <- paste0(base_url, typ)
  }

  # recordedBy
  if (!is.null(recordedBy)) {
    recordedBy <- gsub(" ", "+", recordedBy)
    rby <- url_query(recordedBy, "recordedBy")
    base_url <- paste0(base_url, rby)
  }

  # recordNumber
  if (!is.null(recordNumber)) {
    rn <- url_query(as.character(recordNumber), "recordNumber")
    base_url <- paste0(base_url, rn)
  }

  # yearCollected
  if (!is.null(yearCollected)) {
    yec <- url_query(as.character(yearCollected), "yearCollected")
    base_url <- paste0(base_url, yec)
  }

  # locality
  if (!is.null(locality)) {
    locality <- gsub(" ", "+", locality)
    loc <- url_query(locality, "locality")
    base_url <- paste0(base_url, loc)
  }

  # coordinates
  if (!is.null(coordinates)) {
    xy <- url_query(coordinates, "coordinates")
    base_url <- paste0(base_url, xy)
  }

  # occurrenceRemarks
  if (!is.null(occurrenceRemarks)) {
    occurrenceRemarks <- gsub(" ", "+", occurrenceRemarks)
    or <- url_query(occurrenceRemarks, "occurrenceRemarks")
    base_url <- paste0(base_url, or)
  }

  # barcode
  if (!is.null(barcode)) {
    bc <- url_query(barcode, "barcode")
    base_url <- paste0(base_url, bc)
  }

  # bbox
  if (!is.null(bbox)) {
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
              stop("'bbox' must represent a valid rectangle: longitude1 < longitude2 and latitude1 < latitude2")
            }
          } else {
            stop("'bbox' coordinates must be within valid ranges: longitude (-180 to 180), latitude (-90 to 90)")
          }
        } else {
          stop("'bbox' must contain only numeric coordinates separated by '+'")
        }
      } else {
        stop("'bbox must contain exactly 4 coordinates in format: 'longitude1+latitude1+longitude2+latitude2'")
      }
  }

  # landuse_1
  if (!is.null(landuse_1)) {
    landuse_codes <- strsplit(landuse_1, ",")[[1]]
    if (all(grepl("^[0-9]+$", landuse_codes))) {
      lu1 <- url_query(landuse_1, "landuse_1")
      base_url <- paste0(base_url, lu1)
    } else {
      stop("'landuse_1' must contain comma-separated numeric codes (e.g., '23,33')")
    }
  }

  # landuse_year_1
  if (!is.null(landuse_year_1)) {
    if (landuse_year_1 >= 1985 && landuse_year_1 <= 2021) {
      ly1 <- url_query(as.character(landuse_year_1), "landuse_year_1")
      base_url <- paste0(base_url, ly1)
    } else {
      stop("'landuse_year_1' must be a year between 1985 and 2021")
    }
  }

  # landuse_2
  if (!is.null(landuse_2)) {
    landuse_codes <- strsplit(landuse_2, ",")[[1]]
    if (all(grepl("^[0-9]+$", landuse_codes))) {
      lu2 <- url_query(landuse_2, "landuse_2")
      base_url <- paste0(base_url, lu2)
    } else {
       stop("'landuse_2' must contain comma-separated numeric codes (e.g., '15,9,21')")
    }
  }

  # landuse_year_2
  if (!is.null(landuse_year_2)) {
    if (landuse_year_2 >= 1985 && landuse_year_2 <= 2021) {
      ly2 <- url_query(as.character(landuse_year_2), "landuse_year_2")
      base_url <- paste0(base_url, ly2)
    } else {
      stop("'landuse_year_2' must be a year between 1985 and 2021")
    }
  }

  # phonetic
  if (phonetic) {
    base_url <- paste0(base_url, "phonetic/yes/")
  }

  if (!is.null(scope)) {
    sc <- url_query(scope, "scope")
    base_url <- paste0(base_url, sc)
  }

  if (!is.null(synonyms)) {
    sy <- url_query(synonyms, "synonyms")
    base_url <- paste0(base_url, sy)
  }

  if (typus) {
    base_url <- paste0(base_url, "typus/yes/")
  }

  if (images) {
    base_url <- paste0(base_url, "images/yes/")
  }

  if (!is.null(redlist)) {
    rl <- url_query(redlist, "redlist")
    base_url <- paste0(base_url, rl)
  }

  if (!is.null(limit)) {
    mr <- url_query(limit, "limit")
    base_url <- paste0(base_url, mr)
  }

  base_url <- paste0(base_url, "apikey=", key)

  message("Making request to speciesLink...")

  df_json <- jsonlite::fromJSON(base_url)

  n_records <- df_json$numberMatched

  if (n_records > 5000) {
    n_requests <- 1:( ceiling(n_records/5000) - 1 )

    list_urls <- lapply(n_requests, function(x) {
      offset_x <- ifelse(x == 0, 0, (x * 5000))
      paste0(base_url, "&limit=5000", "&offset=", offset_x)
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

    if (!inherits(dir, "character")) {
      stop("'dir' should be a character string")
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

