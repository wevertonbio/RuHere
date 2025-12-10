#' Extract country from coordinates
#'
#' @description
#' Extracts the country for each occurrence record based on coordinates.
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably
#' standardized using `format_columns()`.
#' @param long (character) column name with longitude. Default is
#' 'decimalLongitude'.
#' @param lat (character) column name with latitude. Default is
#' 'decimalLatitude'.
#' @param from (character) whether to extract the country for all records ('all')
#' or only for records missing country information ('na_only'). If 'na_only',
#' you must provide the name of the column with country information. Default is
#' 'all'.
#' @param country_column (character) the column name containing the country.
#' Only applicable if `from = na_only`. Default is NULL.
#' @param output_column (character) column name created in `occ` to store the
#' countries extracted. Default is 'country_xy'.
#' @param append_source (logical) whether to create a new column in `occ` called
#' 'country_source', which indicates whether the country was derived from
#' coordinates. Default is FALSE.
#'
#' @details
#' The countries are extracted from coordinates using a map retrieved from
#' `rnaturalearthdata::map_units110`.
#'
#'
#' @returns
#' The original `occ` data.frame with an additional column containing the
#' countries extracted from coordinates.
#'
#' @importFrom terra unwrap extract
#'
#' @export
#'
#' @examples
#' # Import and standardize GBIF
#' data("occ_gbif", package = "RuHere") #Import data example
#' gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
#' gbif_countries <- country_from_coords(occ = gbif_standardized)
country_from_coords <- function(occ,
                                long = "decimalLongitude",
                                lat = "decimalLatitude",
                                country_column = NULL,
                                from = "all",
                                output_column = "country_xy",
                                append_source = FALSE){

  # ---- ARGUMENT CHECKING ----
  # 1. Check occ (no default, must be provided)
  if (missing(occ) || !inherits(occ, "data.frame")) {
    stop("'occ' must be provided as a data.frame containing occurrence records.", call. = FALSE)
  }
  if (nrow(occ) == 0) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.", call. = FALSE)
  }

  # 2. Check long
  if (!inherits(long, "character") || length(long) != 1) {
    stop("'long' must be a single character string specifying the longitude column.", call. = FALSE)
  }
  if (!long %in% names(occ)) {
    stop(paste0("The column specified in 'long' ('", long, "') was not found in 'occ'."), call. = FALSE)
  }
  if (!inherits(occ[[long]], "numeric")) {
    stop("The 'long' column must be numeric.", call. = FALSE)
  }

  # 3. Check lat
  if (!inherits(lat, "character") || length(lat) != 1) {
    stop("'lat' must be a single character string specifying the latitude column.", call. = FALSE)
  }
  if (!lat %in% names(occ)) {
    stop(paste0("The column specified in 'lat' ('", lat, "') was not found in 'occ'."), call. = FALSE)
  }
  if (!inherits(occ[[lat]], "numeric")) {
    stop("The 'lat' column must be numeric.", call. = FALSE)
  }

  # 4. Check from (default = "all")
  allowed_from <- c("all", "na_only")
  if (!inherits(from, "character") || length(from) != 1 || !from %in% allowed_from) {
    stop("'from' must be a single character string and either 'all' or 'na_only'.", call. = FALSE)
  }

  # 5. Check country_column if from = "na_only" (default = NULL)
  if (from == "na_only") {
    if (!inherits(country_column, "character") || length(country_column) != 1) {
      stop("'country_column' must be a single character string when 'from = \"na_only\"'.", call. = FALSE)
    }
    if (!country_column %in% names(occ)) {
      stop(paste0("The column specified in 'country_column' ('", country_column, "') was not found in 'occ'."), call. = FALSE)
    }
  }

  # 6. Check output_column (default must exist)
  if (!inherits(output_column, "character") || length(output_column) != 1) {
    stop("'output_column' must be a single character string specifying the name of the new column.", call. = FALSE)
  }
  # if (output_column %in% names(occ)) {
  #   warning(paste0("The column '", output_column, "' already exists and will be overwritten."))
  # }

  # 7. Check append_source (default = FALSE)
  if (!inherits(append_source, "logical") || length(append_source) != 1) {
    stop("'append_source' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  if(append_source && is.null(country_column)){
    stop("If you set 'append_source' to TRUE, you must provide a 'country_columns'")
  }

  if(append_source && from != "na_only"){
    stop("If you set 'append_source' to TRUE, 'from' must be 'na_only'")
  }

  # Convert to dataframe if necessary
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)


  # Get map of world
  w <- terra::unwrap(getExportedValue("RuHere", "world"))

  # Extract
  if(from == "all"){
    occ[[output_column]] <- NA
    country_ext <- terra::extract(w, occ[, c(long, lat)])
    country_ext <- country_ext[!duplicated(country_ext$id.y), ]
    occ[[output_column]] <- country_ext$name
    }

  if(from == "na_only"){
    na_country <- which(is.na(occ[[country_column]]))
    country_ext <- terra::extract(w, occ[na_country, c(long, lat)])
    country_ext <- country_ext[!duplicated(country_ext$id.y), ]
    occ[na_country, output_column] <- country_ext$name
  }


  #Reorder columns, if necessary
  if(!is.null(country_column) && country_column != output_column){
    occ <- relocate_after(occ, output_column, country_column)}

  if(append_source && country_column == output_column){
    occ$country_source <- NA
    occ <- relocate_after(occ, "country_source", output_column)
    to_append <- intersect(na_country, which(!is.na(occ[[output_column]])))
    occ$country_source[to_append] <- "coords"
  }

  return(occ)

}
