# Perguntas:
# Precisa de e-mail?
# É possível transformar o argumento "rq" em "species"?
# Talvez deixar search_records como padrão?


get_idigbio <- function(type = c("api_records, search_records, search_media"),
                        email = NULL, rq = NULL, fields = NULL,
                        max_items = NULL, limit = NULL, offset = NULL,
                        sort = NULL, mq = NULL, dir = "results/",
                        filename = "idigbio_output", save = TRUE,
                        compress = FALSE, ...) {

  if (is.null(rq) && is.null(mq)) {
    stop("rq/mq are required for searching in iDigBio!")
  }

  if (type == "api_records") {

    warning("'api_records' is the recommended mode for very large datasets (millions of records). The synchronous types ('search_records'/'search_media') offer greater flexibility with arguments like pagination ('limit', 'offset') and field selection ('fields'). Consider using them for smaller, more precise queries.")

    invalid_args <- c()
    if (!is.null(max_items) && !inherits(max_items, "numeric")) {
      invalid_args <- c(invalid_args, "max_items")
    }

    if (!is.null(limit)) invalid_args <- c(invalid_args, "limit")
    if (!is.null(offset)) invalid_args <- c(invalid_args, "offset")
    if (!is.null(sort)) invalid_args <- c(invalid_args, "sort")
    if (!is.null(mq)) invalid_args <- c(invalid_args, "mq")

    if (length(invalid_args) > 0) {
        warning("The following arguments are not valid for 'api_records': ",
                paste(invalid_args, collapse = ", "))
    }

    query_json <- jsonlite::toJSON(rq, auto_unbox = TRUE)
    query_encoded <- utils::URLencode(query_json, reserved = TRUE)

    base_url <- "https://api.idigbio.org/v2/download/"

    base_url <- paste0(base_url, "?rq=", query_encoded)

    if (!is.null(email) && (!is.character(email) || !grepl("@", email))) {
      stop("email should be a valid address")
    }

    if (!is.null(email)) base_url <- paste0(base_url, "&email=", email)

    response <- httr::GET(base_url)

    if (httr::status_code(response) != 200) {
      stop("Error in request: ", httr::content(response, "text"))
    }

    result <- httr::content(response, as = "parsed")

    if (is.null(result$status_url)) {
      stop("No status URL returned. Response: ", jsonlite::toJSON(result, auto_unbox = TRUE))
    }

    status_url <- result$status_url

    message("Still processing...")
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
    }

    options(timeout = 3600)

    download_url <- status$download_url

    if (is.null(download_url)) {
      stop("Error during download: please try again.")
    }

    temp_dir <- tempdir()

    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE)
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

    unlink(zip_file)
    unlink(extract_dir, recursive = TRUE)
  }

  if (type == "search_records") {

    invalid_args <- c()
    if (!is.null(email)) invalid_args <- c(invalid_args, "email")
    if (!is.null(mq)) invalid_args <- c(invalid_args, "mq")
    if (length(invalid_args) > 0) {
        warning("The following arguments are not valid for 'search_records': ",
                paste(invalid_args, collapse = ", "))
    }

    warning("Synchronous searches are strictly limited to a MAXIMUM of 100,000 records ('max_items'). You cannot retrieve the remaining data using 'limit' or 'offset' for queries exceeding this cap. For large volumes, you must use 'api_records'.")

    if (!is.null(max_items)) {
      if(!inherits(max_items, "numeric")) {
        stop("max_items should be numeric")
      }
    } else {
      max_items <- 1e+05
    }

    if (!is.null(limit)) {
      if(!inherits(limit, "numeric")) {
        stop("limit should be numeric")
      }
    } else {
      limit <- 0
    }

    if (!is.null(offset)) {
      if(!inherits(offset, "numeric")) {
        stop("offset should be numeric")
      }
    } else {
      offset <- 0
    }

    if (!is.null(sort)) {
      if(!inherits(sort, "logical")) {
        stop("sort should be logical")
      }
    } else {
      sort <- FALSE
    }

    if (is.null(fields)) fields <- FALSE

    if (is.null(mq)) mq <- FALSE

    df <- ridigbio::idig_search_records(rq = rq, fields = fields,
                                        max_items = max_items, limit = limit,
                                        offset = offset, sort = sort, ...)
  }

  if (type == "search_media") {

    invalid_args <- c()
    if (!is.null(email)) invalid_args <- c(invalid_args, "email")
    if (length(invalid_args) > 0) {
        warning("The following arguments are not valid for 'search_media': ",
                paste(invalid_args, collapse = ", "))
    }

    warning("Synchronous searches are strictly limited to a MAXIMUM of 100,000 records ('max_items'). You cannot retrieve the remaining data using 'limit' or 'offset' for queries exceeding this cap. For large volumes, you must use 'api_records'.")

    if (!is.null(max_items)) {
      if(!inherits(max_items, "numeric")) {
        stop("max_items should be numeric")
      }
    } else {
      max_items <- 1e+05
    }

    if (!is.null(limit)) {
      if(!inherits(limit, "numeric")) {
        stop("limit should be numeric")
      }
    } else {
      limit <- 0
    }

    if (!is.null(offset)) {
      if(!inherits(offset, "numeric")) {
        stop("offset should be numeric")
      }
    } else {
      offset <- 0
    }

    if (!is.null(sort)) {
      if(!inherits(sort, "logical")) {
        stop("sort should be logical")
      }
    } else {
      sort <- FALSE
    }

    if (is.null(fields)) fields <- FALSE

    if (is.null(mq)) mq <- FALSE

    df <- ridigbio::idig_search_media(rq = rq, mq = mq, fields = fields,
                                      max_items = max_items, limit = limit,
                                      offset = offset, sort = sort, ...)
  }

  if (save) {

    for (i in 1:length(names(df))) {
      if (is.list(df[[i]]) && !is.data.frame(df[[i]])) {
        df[[i]] <- vapply(df[[i]], function(x) {
          if (is.null(x) || length(x) == 0) {
            return(NA_character_)
          } else {
            return(paste(unlist(x), collapse = "; "))
          }
        }, FUN.VALUE = character(1), USE.NAMES = FALSE)
      }
    }

    if (!inherits(dir, "character")) {
      stop("'dir' should be a character string")
    }

    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE)
    }

    if (!inherits(filename, "character")) {
      stop("'filename' must be a character string")
    }

    if (compress) {
      fullname <- paste0(dir, filename, ".csv.zip")
      data.table::fwrite(df, file = fullname, compress = "gzip")
    } else {
      fullname <- paste0(dir, filename, ".csv")
      data.table::fwrite(df, file = fullname)
    }

  }

  return(df)
}

# # # Test
# res_test <- get_idigbio(
#     type = "api_records",
#     rq=list(genus="acer", country = "brazil"),
#     compress = T
# )
