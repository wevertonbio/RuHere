#' Submit a request to download occurrence data from GBIF.
#'
#' @usage request_gbif(gbif_info, hasCoordinate = TRUE,
#'                     hasGeospatialIssue = FALSE, format = "DWCA",
#'                     gbif_user = NULL, gbif_pwd = NULL, gbif_email = NULL,
#'                     additional_predicates = NULL)
#'
#' @param gbif_info an object of class 'gbif_info' resulted by the
#' `prepare_gbif_download()` function.
#' @param hasCoordinate (logical) whether to retrieve only records with coordinates. Default is TRUE.
#' @param hasGeospatialIssue (logical) whether to retrieve records identified with geospatial issue. Default is FALSE.
#' @param format (character) the download format. Options available are 'DWCA',
#' 'SIMPLE_CSV', or 'SPECIES_LIST', Default is DWCA'.
#' @param gbif_user (character) user name within GBIF's website. Default is
#' NULL, meaning it will try to obtain this information from the R enviroment.
#' (check `set_gbif_credentials()`) for more details.
#' @param gbif_pwd (character) user password within GBIF's website. Default is
#' NULL, meaning it will try to obtain this information from the R enviroment.
#' @param gbif_email (character) user email within GBIF's website. Default is
#' NULL, meaning it will try to obtain this information from the R enviroment.
#' @param additional_predicates (character or occ_predicate) additional
#' supported predicates that can be combined to build more complex download requests. See
#' `rgbif::pred()` for details.
#'
#' @details
#' You can use the object returned by this function to check the download
#' request progress with `rgbif::occ_download_wait()`
#'
#' @note
#' This function requires an active internet connection and valid GBIF
#' credentials.
#'
#' @returns
#' A download request key returned by the GBIF API, which can be used to monitor
#' or retrieve the download.
#'
#' @importFrom rgbif pred_and pred_in pred occ_download
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Prepare data to request GBIF download
#' gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
#' # Submit a request to download occurrences
#' gbif_requested <- request_gbif(gbif_info = gbif_prepared)
#' # Check progress
#' rgbif::occ_download_wait(gbif_requested)
#' }
request_gbif <- function(gbif_info, hasCoordinate = TRUE,
                         hasGeospatialIssue = FALSE, format = "DWCA",
                         gbif_user = NULL, gbif_pwd = NULL, gbif_email = NULL,
                         additional_predicates = NULL){
  # Check data
  if (!inherits(gbif_info, "gbif_info"))
    stop("'gbif_info' must be an object of class 'gbif_info', not ", class(gbif_info), call. = FALSE)

  if (!inherits(hasCoordinate, "logical") || length(hasCoordinate) != 1)
    stop("'hasCoordinate' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)

  if (!inherits(hasGeospatialIssue, "logical") || length(hasGeospatialIssue) != 1)
    stop("'hasGeospatialIssue' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)

  if (!inherits(format, "character") || length(format) != 1)
    stop("'format' must be a single character value.", call. = FALSE)

  valid_formats <- c("DWCA", "SIMPLE_CSV", "SPECIES_LIST")
  if (!toupper(format) %in% valid_formats)
    stop("'format' must be one of: ", paste(valid_formats, collapse = ", "),
         call. = FALSE)

  if (!is.null(gbif_user) && !inherits(gbif_user, "character"))
    stop("'gbif_user' must be a character value or NULL, not ", class(gbif_user), call. = FALSE)

  if (!is.null(gbif_pwd) && !inherits(gbif_pwd, "character"))
    stop("'gbif_pwd' must be a character value or NULL, not ", class(gbif_pwd),
         call. = FALSE)

  if (!is.null(gbif_email) && !inherits(gbif_email, "character"))
    stop("'gbif_email' must be a character value or NULL, not ", class(gbif_email),
         call. = FALSE)

  if (!is.null(additional_predicates) &&
      !inherits(additional_predicates, c("character", "occ_predicate")))
    stop("'additional_predicates' must be a object of class 'character' or 'occ_predicate' or NULL, not ", class(additional_predicates), call. = FALSE)

  # Get key
  k <- gbif_info$usageKey

  if (length(k) == 0 || all(is.na(k)))
    stop("The 'gbif_info' object has only empty usage keys.", call. = FALSE)

  # Get list of arguments
  p_list <- rgbif::pred_and(rgbif::pred_in("taxonKey", k), #Taxonkey
                 rgbif::pred("hasCoordinate", hasCoordinate),
                 rgbif::pred("hasGeospatialIssue", hasGeospatialIssue))

  if(!is.null(additional_predicates)){
    # If additional_predicates is a single pred
    if(inherits(additional_predicates[[1]], "character")){
      p_list2 <- c(p_list, list(additional_predicates))
      # If additional_predicates is a multiple pred
    } else if (inherits(additional_predicates[[1]], "occ_predicate")){
      p_list2 <- c(p_list, additional_predicates)
    }
    p_list <- do.call(
      what = rgbif::pred_and,
      args = p_list2)
  }

  # Get request
  occ_request <- rgbif::occ_download(
    p_list,
    format = format,
    user = gbif_user,
    pwd = gbif_pwd,
    email = gbif_email) # Other arguments
  request_key <- occ_request[1]
  class(request_key) <- "request_key"

  return(request_key)
}
