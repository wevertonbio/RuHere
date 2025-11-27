#' Extract state from coordinates
#'
#' @description
#' Extracts the state for each occurrence record based on coordinates.
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably
#' standardized using `format_columns()`.
#' @param long (character) column name with longitude. Default is
#' 'decimalLongitude'.
#' @param lat (character) column name with latitude. Default is
#' 'decimalLatitude'.
#' @param from (character) whether to extract the state for all records ('all')
#' or only for records missing state information ('na_only'). If 'na_only',
#' you must provide the name of the column with state information. Default is
#' 'all'.
#' @param state_column (character) the column name containing the state. Only
#' applicable if `from = na_only`. Default is NULL.
#' @param output_column (character) column name created in `occ` to store the
#' states extracted. Default is 'state_xy'.
#' @param append_source (logical) whether to create a new column in `occ` called
#' 'state_source', which indicates whether the state was derived from
#' coordinates. Default is FALSE.
#'
#' @details
#' The states are extracted from coordinates using a map retrieved from
#' `rnaturalearthdata::states50`.
#'
#' @returns
#' The original `occ` data.frame with an additional column containing the
#' states extracted from coordinates.
#'
#' @importFrom terra vect extract
#' @export
#'
#' @examples
#' # Import and standardize GBIF
#' data("occ_gbif", package = "RuHere") #Import data example
#' gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
#' gbif_states <- states_from_coords(occ = gbif_standardized)
states_from_coords <- function(occ,
                               long = "decimalLongitude",
                               lat = "decimalLatitude",
                               from = "all", #or na_only
                               state_column = "stateProvince",
                               output_column = "state_xy",
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

  # 5. Check state_column if from = "na_only" (default = NULL)
  if (from == "na_only") {
    if (!inherits(state_column, "character") || length(state_column) != 1) {
      stop("'state_column' must be a single character string when 'from = \"na_only\"'.", call. = FALSE)
    }
    if (!state_column %in% names(occ)) {
      stop(paste0("The column specified in 'state_column' ('", state_column, "') was not found in 'occ'."), call. = FALSE)
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

  if(append_source && is.null(state_column)){
    stop("If you set 'append_source' to TRUE, you must provide a 'state_column'")
  }

  if(append_source && from != "na_only"){
    stop("If you set 'append_source' to TRUE, 'from' must be 'na_only'")
  }


  # Convert to dataframe if necessary
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)


  # Get map of states
  s <- terra::vect(system.file("extdata/states.shp", package = "RuHere"))

  # Extract
  if(from == "all"){
    occ[[output_column]] <- NA
    occ[[output_column]] <- terra::extract(s, occ[, c(long, lat)])[[2]]
  }

  if(from == "na_only"){
    na_state <- which(is.na(occ[[state_column]]) | occ[[state_column]] == "")
    occ[na_state, output_column] <- terra::extract(s,
                                                   occ[na_state,
                                                       c(long, lat)])[[2]]
  }

  #Reorder columns, if necessary
  if(state_column != output_column){
    occ <- relocate_after(occ, output_column, state_column)}

  if(append_source && state_column == output_column){
    occ$state_source <- NA
    occ <- relocate_after(occ, "state_source", output_column)
    to_append <- intersect(na_state, which(!is.na(occ[[output_column]])))
    occ$state_source[to_append] <- "coords"
  }

  return(occ)
}
