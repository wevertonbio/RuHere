
#### Download de registros do iDigBio ####
# https://www.idigbio.org/

# Carregar pacotes
library(ridigbio)

# Primeiro, vamos especificar as colunas para retornar
f <- c("basisofrecord", "canonicalname", "scientificname",
       "catalognumber", "collectioncode", "collectionid",
       "collector", "coordinateuncertainty", "country", "county",
       "stateprovince", "locality", "municipality", "datasetid",
       "datecollected", "geopoint","datasetid")

# Baixar dados
occ_digbio <- idig_search_records(rq=list(scientificname = sp,
                                          kingdom = "plantae"),
                                  fields = f)


get_idigbio <- function(type = "records", email, rq = NULL, mq = NULL, 
                        fields = NULL, limit = NULL, fields_exclude = NULL,
                        no_attribution = TRUE,
                        offset = 0, sort = NULL, dir = "results/", 
                        filename = "output", save = TRUE, file.format = "csv",
                        compress = FALSE, ...) {

  all_fields <- c("uuid", "occurrenceid", "catalognumber", "family", "genus", 
                  "scientificname", "country", "stateprovince", "geopoint", 
                  "data.dwc:eventDate", "data.dwc:year", "data.dwc:month", 
                  "data.dwc:day", "collector", "recordset")

  query <- list(offset = offset)

  if (!is.null(sort)) {
    if (inherits(sort, "character")) {
      query$sort <- sort
    } else {
      stop("sort must be a character (or vector of characters)")
    }
  }

  if (is.null(rq) && is.null(mq)) {
    stop("rq/mq are required for searching in iDigBio!")
  } else if (!(inherits(rq, "list") || !(inherits(mq, "list")))) {
    stop("rq/mq must be lists")
  } else if (!is.null(mq)) {
    query$mq <- mq
  } else if (!is.null(rq)) {
    query$rq <- rq
  }

  if (is.null(query$fields_exclude)) {
    query$fields_exclude <- c("indexData", "flags")
  } else {
    query$fields_exclude <- c(query$fields_exclude, "indexData", "flags")
  }

  if (!is.null(fields)) {
    if (!inherits(fields, "character")) {
      stop("fields must be a character vector")
    }
    if (!(length(fields) == 1 && fields == "all")) {
      query$fields <- as.list(fields)
    }
    if (is.null(fields) || (length(fields) == 1 && fields == "all")) {
      fields <- all_fields
    }
  }

  if (!is.null(limit)) {
    if (inherits(limit, "numeric")) {
      query$limit <- limit
    } else {
      stop("limit must be numeric")
    }
  }

  if (no_attribution == TRUE) {
    if (inherits(no_attribution, "logical")) {
      query$no_attribution <- TRUE
    } else {
      stop("no_attribution must be TRUE or FALSE")
    }
  }
  
  query_json <- jsonlite::toJSON(query, auto_unbox = TRUE)
  query_encoded <- utils::URLencode(query_json, reserved = TRUE)
  
  base_url <- paste0("https://api.idigbio.org/v2/download/?rq=", query_encoded)
  
  if (!is.null(email)) {
    if (!is.character(email) || !grepl("@", email)) {
      stop("email must be a valid email address")
    }
    base_url <- paste0(base_url, "&email=", utils::URLencode(email))
  }
    
  response <- httr::GET(base_url)
  
  if (httr::status_code(response) != 200) {
    stop("Error in request: ", httr::content(response, "text"))
  }
  
  result <- httr::content(response, as = "parsed")
  
  if (is.null(result$status_url)) {
    stop("No status URL returned. Response: ", jsonlite::toJSON(result, auto_unbox = TRUE))
  }
  
  status_url <- result$status_url
  
  while (TRUE) {    
    status_response <- httr::GET(status_url)
    status <- httr::content(status_response, as = "parsed")
    
    if (!is.null(status$complete) && status$complete == TRUE) {
      message("Processing complete!")
      break
    }
    
    if (!is.null(status$error)) {
      stop("Error during processing: ", status$error)
    }
    
    message("Still processing...")
  }
  
  download_url <- status$download_url
  
  if (is.null(download_url)) {
    stop("Error during download: please try again.")
  }

  zip_file <- tempfile(fileext = ".zip")
  download.file(download_url, destfile = zip_file, mode = "wb", quiet = FALSE)
  
  extract_dir <- tempdir()
  unzip(zip_file, exdir = extract_dir)

  files <- list.files(extract_dir, pattern = "occurrence\\.(txt|csv)", 
                      full.names = TRUE, recursive = TRUE)
  
  if (length(files) == 0) {
    stop("Occurrence file not found.")
  }
  
  occ_file <- files[1]
  
  df <- data.table::fread(occ_file, encoding = "UTF-8")
  
  if (save) {
    
    if (!file.format %in% c("csv", "rds")) {
      stop("file.format must be 'csv' or 'rds'")
    }
    
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }
    
    if (file.format == "csv") {
      if (compress) {
        fullname <- paste0(dir, filename, ".csv.zip")
        data.table::fwrite(df, file = fullname, compress = "gzip")
      } else {
        fullname <- paste0(dir, filename, ".csv")
        data.table::fwrite(df, file = fullname)
      }
    }
    
    if (file.format == "rds") {
      fullname <- paste0(dir, filename, ".rds")
      if (compress) {
        saveRDS(df, file = fullname, compress = "gzip")
      } else {
        saveRDS(df, file = fullname, compress = FALSE)
      }
    }
  }
  
  unlink(zip_file)
  
  return(df)
}

    
# Exemplo de uso:
# rq <- list(
#   scientificname = "Panthera onca",
#   country = "brazil"
# )
#
# occ <- get_idigbio_download(
#   rq = rq,
#   email = "seu_email@exemplo.com",  # Opcional
#   dir = "results/",
#   filename = "panthera_onca",
#   save = TRUE,
#   file.format = "csv",
#   compress = TRUE,
#   check_interval = 30,     # Checar status a cada 30 segundos
#   max_wait_hours = 24      # Tempo máximo de espera
# )