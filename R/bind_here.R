#' Bind occurrences after standardizing columns
#'
#' @description
#' Combines multiple occurrence data frames (for example, from GBIF,
#' SpeciesLink, BIEN, or iDigBio) into a single standardized dataset. This is
#' particularly useful after using `format_columns()` to ensure column
#' compatibility across data sources.
#'
#' @param ... (data.frame) two or more data frames with occurrence records to
#' combine.
#' @param fill (logical) whether to fills missing columns with `NA`. Default is
#' FALSE.
#'
#' @returns
#' A `data.frame` containing all occurrence records combined.
#'
#' @details
#' When `fill = TRUE`, columns not shared among the input data frames are added
#' and filled with `NA`, ensuring that all columns align before binding.
#' Internally, this function uses `data.table::rbindlist()` for efficient row
#' binding.
#'
#' @export
#'
#' @importFrom data.table rbindlist
#'
#' @examples
#' # Import and standardize GBIF
#' data("occ_gbif", package = "RuHere") #Import data example
#' gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
#' # Import and standardize SpeciesLink
#' data("occ_splink", package = "RuHere") #Import data example
#' splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
#' # Import and standardize BIEN
#' data("occ_bien", package = "RuHere") #Import data example
#' bien_standardized <- format_columns(occ_bien, metadata = "bien")
#' # Import and standardize idigbio
#' data("occ_idig", package = "RuHere") #Import data example
#' idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
#' # Merge all
#' all_occ <- bind_here(gbif_standardized, splink_standardized,
#'                       bien_standardized, idig_standardized)
#'
bind_here <- function(...,
                      fill = FALSE){

  # ---- Argument checks ----

  # 1. Check fill
  if (!inherits(fill, "logical") || length(fill) != 1) {
    stop("'fill' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # 2. Check that at least one object is provided
  l <- list(...)
  if (length(l) == 0) {
    stop("At least one data.frame must be provided to 'bind_here()'.", call. = FALSE)
  }

  # 3. Check that all elements are data.frames or data.tables
  if (!all(vapply(l, function(x) inherits(x, c("data.frame", "data.table")), logical(1)))) {
    stop("All inputs in '...' must be data.frames or data.tables.", call. = FALSE)
  }

  l <- list(...)
  l <- data.table::rbindlist(l)
  if(inherits(l, "data.table"))
    l <- as.data.frame(l)
  return(l)
}
