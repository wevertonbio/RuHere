#' @importFrom DBI dbDriver dbConnect dbDisconnect dbSendQuery dbFetch dbClearResult dbHasCompleted dbGetQuery
#' @importFrom sf st_as_sf
#' @importFrom RPostgreSQL PostgreSQL
#'
#' @noRd
BIEN_sql <- function (query, view_full_occurrence_individual = NULL,
            agg_traits = NULL, species_by_political_division = NULL,
            bien_species_all = NULL, ranges = NULL, 
            bien_taxonomy = NULL, phylogeny = NULL, 
            bien_metadata = NULL, plot_metadata = NULL,
            analytical_stem = NULL, datasource = NULL,
            centroid = NULL, limit = NULL, return.query = FALSE,
            schema = NULL, print.query = FALSE, fetch.query = TRUE,
            record_limit = 10000) {

  if (!inherits(query, c("character", "NULL"))) {
    stop(sys.call()[- 1], " should be character", call. = FALSE)
  }
  if (print.query) {
    query <- gsub(pattern = "\n", replacement = "", query)
    query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, 
      perl = TRUE)
    print(query)
  }
  if (!is.null(schema)) {
    view_full_occurrence_individual <- paste(schema, "view_full_occurrence_individual", 
      sep = ".")
    agg_traits <- paste(schema, "agg_traits", sep = ".")
    species_by_political_division <- paste(schema, "species_by_political_division", 
      sep = ".")
    bien_species_all <- paste(schema, "bien_species_all", 
      sep = ".")
    ranges <- paste(schema, "ranges", sep = ".")
    bien_taxonomy <- paste(schema, "bien_taxonomy", sep = ".")
    phylogeny <- paste(schema, "phylogeny", sep = ".")
    bien_metadata <- paste(schema, "bien_metadata", sep = ".")
    plot_metadata <- paste(schema, "plot_metadata", sep = ".")
    analytical_stem <- paste(schema, "analytical_stem", sep = ".")
    datasource <- paste(schema, "datasource", sep = ".")
    centroid <- paste(schema, "centroid", sep = ".")
  }
  if (!is.null(view_full_occurrence_individual)) {
    query <- gsub(pattern = "(?<!as |AS )(?<!\\S)view_full_occurrence_individual(?!\\S)", 
      replacement = view_full_occurrence_individual, x = query, 
      perl = TRUE)
  }
  if (!is.null(plot_metadata)) {
    query <- gsub(pattern = "(?<!as |AS )(?<!\\S)plot_metadata(?!\\S)", 
      replacement = plot_metadata, x = query, perl = TRUE)
  }
  if (!is.null(analytical_stem)) {
    query <- gsub(pattern = "(?<!as |AS )(?<!\\S)analytical_stem(?!\\S)", 
      replacement = analytical_stem, x = query, perl = TRUE)
  }
  if (!is.null(agg_traits)) {
    query <- gsub(pattern = "agg_traits", replacement = agg_traits, 
      x = query)
  }
  if (!is.null(species_by_political_division)) {
    query <- gsub(pattern = "species_by_political_division", 
      replacement = species_by_political_division, x = query)
  }
  if (!is.null(bien_species_all)) {
    query <- gsub(pattern = "bien_species_all", replacement = bien_species_all, 
      x = query)
  }
  if (!is.null(ranges)) {
    query <- gsub(pattern = "ranges", replacement = ranges, 
      x = query)
  }
  if (!is.null(bien_taxonomy)) {
    query <- gsub(pattern = "bien_taxonomy", replacement = bien_taxonomy, 
      x = query)
  }
  if (!is.null(phylogeny)) {
    query <- gsub(pattern = "\\<phylogeny\\>", replacement = phylogeny, 
      x = query)
  }
  if (!is.null(bien_metadata)) {
    query <- gsub(pattern = "\\<bien_metadata\\>", replacement = bien_metadata, 
      x = query)
  }
  if (!is.null(datasource)) {
    query <- gsub(pattern = "(?<=\\s)datasource(?=\\s)", 
      replacement = datasource, x = query, perl = TRUE)
  }
  if (!is.null(centroid)) {
    query <- gsub(pattern = "(?<=\\s)centroid(?=\\s)", replacement = datasource, 
      x = query, perl = TRUE)
  }
  if (!is.null(limit)) {
    query <- gsub(pattern = " ;", replacement = paste(" LIMIT ", 
      limit, ";"), x = query)
  }
  host <- "vegbiendev.nceas.ucsb.edu"
  dbname <- "public_vegbien"
  user <- "public_bien"
  password <- "bien_public"
  drv <- DBI::dbDriver("PostgreSQL")
  if (return.query) {
    query <- gsub(pattern = "\n", replacement = "", query)
    query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, 
      perl = TRUE)
    return(query)
  }
  con <- DBI::dbConnect(drv, host = host, dbname = dbname, user = user, 
    password = password)
  if ("error" %in% class(con)) {
    message("\n There was an error connecting to the BIEN database.")
    rm(con)
    return(invisible(NULL))
  }
  if (fetch.query) {
    suppressWarnings(res <- tryCatch(expr = DBI::dbSendQuery(conn = con, 
      statement = query), error = function(e) {
      e
    }))
    if ("error" %in% class(res)) {
      message("\n There was an error in the SQL query or the SQL query could not be sent (e.g., due to connection issues).")
      suppressWarnings(db_cleared <- tryCatch(expr = DBI::dbClearResult(res = res), 
        error = function(e) {
          e
        }))
      dbDisconnect(con)
      return(invisible(NULL))
    }
    df <- NULL
    batch <- 1
    while (!DBI::dbHasCompleted(res)) {
      message("Getting page ", batch, " of records")
      df_x <- tryCatch(expr = DBI::dbFetch(res = res, n = record_limit), 
        error = function(e) {
          e
        })
      if ("error" %in% class(df_x)) {
        message("Error fetching page ", batch, " of records.")
        DBI::dbClearResult(res)
        return(invisible(NULL))
      }
      df <- rbind(df_x, df)
      batch = batch + 1
    }
    suppressWarnings(db_cleared <- tryCatch(expr = DBI::dbClearResult(res = res), 
      error = function(e) {
        e
      }))
  }
  else {
    df <- DBI::dbGetQuery(con, statement = query)
  }
  DBI::dbDisconnect(con)
  if (print.query) {
    query <- gsub(pattern = "\n", replacement = "", query)
    query <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", query, 
      perl = TRUE)
    print(query)
  }
  if ("error" %in% class(df)) {
    message("\nThe query was properly executed, but there was a problem retreiving the query results. This may be due to internet connection issues.")
    return(invisible(NULL))
  }
  else {
    return(df)
  }
}

#' @noRd
BIEN_ranges_load_species <- function(species, ...) {

  if (!inherits(species, c("character", "NULL"))) {
    stop(sys.call()[- 1], " should be character", call. = FALSE)
  }
  species <- gsub(" ", "_", species)
  query <- paste("SELECT ST_AsText(geom) as geometry,species,gid FROM ranges WHERE species in (", 
  paste(shQuote(species, type = "sh"), collapse = ", "), 
  ") ;")

  df <- BIEN_sql(query, ...)
  if (length(df) == 0) {
      message("No species matched")
      return(invisible(NULL))
  }
  else {
      poly <- sf::st_as_sf(x = df, wkt = "geometry", crs = "epsg:4326")
      return(poly)
  }
}

