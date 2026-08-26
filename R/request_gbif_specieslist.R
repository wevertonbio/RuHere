#' Download a list of species recorded inside a spatial polygon from GBIF.
#'
#' @description
#' Download a list of species recorded inside a spatial polygon from GBIF, with
#' support for optional higher-taxon filtering.
#'
#' @param spatial_polygon an object of class `SpatVector` representing the area
#' of interest.
#' @param kingdom,phylum,class,order,family,genus,species (character) optional
#' taxonomic filters. Default is NULL.
#' @param tolerance (numeric) tolerance in degrees for geometry simplification.
#' Default is 0.01.
#' @param gbif_user (character) user name within GBIF's website. Default is
#' NULL, meaning it will try to obtain this information from the R enviroment.
#' (check `set_gbif_credentials()`) for more details.
#' @param gbif_pwd (character) user password within GBIF's website. Default is
#' NULL, meaning it will try to obtain this information from the R enviroment.
#' @param gbif_email (character) user email within GBIF's website. Default is
#' NULL, meaning it will try to obtain this information from the R enviroment.
#' @param verbose (logical) if TRUE, prints messages about the progress. Default is
#' `FALSE`.
#'
#' @details
#' This function converts the input \code{SpatVector} polygon into Well-Known
#' Text (WKT) format and submits an asynchronous query using GBIF's
#' \code{SPECIES_LIST} download format.
#'
#' You can use the object returned by this function to check the download
#' request progress with `rgbif::occ_download_wait()`
#'
#' @note
#' This function requires an active internet connection and valid GBIF
#' credentials. Set them in advance using `set_gbif_credentials()`.
#'
#' @returns
#' A download request key returned by the GBIF API, which can be used to monitor
#' or retrieve the download of the species list.
#'
#' @importFrom terra crs project simplifyGeom geom crds
#' @importFrom rgbif pred name_backbone occ_download
#' @importFrom utils tail
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Set your GBIF credentials (required before running this function)
#' # set_gbif_credentials(gbif_username = "your_username",
#' #                       gbif_email = "your_email@example.com",
#' #                       gbif_password = "your_password")
#'
#' # Create a sample polygon
#' coords <- matrix(c(-48, -16, -47, -16, -47, -15, -48, -15, -48, -16),
#'                  ncol = 2, byrow = TRUE)
#' poly <- vect(coords, type = "polygons", crs = "EPSG:4326")
#'
#' # Submit a request to download list of Malvaceae species in the area
#' gbif_requested_sl <- request_gbif_specieslist(spatial_polygon = poly,
#'                                             family = "Malvaceae")
#' # Monitor download progress
#' rgbif::occ_download_wait(gbif_requested_sl)
#'
#' # Import completed species list
#' sl <- import_gbif(gbif_requested_sl, select_columns = FALSE)
#' }
request_gbif_specieslist <- function(spatial_polygon,
                                     kingdom = NULL,
                                     phylum = NULL,
                                     class = NULL,
                                     order = NULL,
                                     family = NULL,
                                     genus = NULL,
                                     species = NULL,
                                     tolerance = 0.01,
                                     gbif_user = NULL,
                                     gbif_pwd = NULL,
                                     gbif_email = NULL,
                                     verbose = TRUE) {

  # 1. Check spatial_polygon
  if (!inherits(spatial_polygon, "SpatVector")) {
    stop("'spatial_polygon' must be an object of class 'SpatVector', not ",
         class(spatial_polygon)[1], call. = FALSE)
  }

  # 2. Check taxonomic arguments
  tax_args <- list(kingdom = kingdom, phylum = phylum, class = class,
                   order = order, family = family, genus = genus, species = species)

  for (arg_name in names(tax_args)) {
    val <- tax_args[[arg_name]]
    if (!is.null(val) && (!inherits(val, "character") || length(val) != 1)) {
      stop(sprintf("'%s' must be a single character value or NULL, not %s.",
                   arg_name, class(val)[1]), call. = FALSE)
    }
  }

  # 3. Check logical filters and flags
  if (!inherits(verbose, "logical") || length(verbose) != 1) {
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # 4. Check numeric tolerance
  if (!inherits(tolerance, "numeric") || length(tolerance) != 1 || tolerance <= 0) {
    stop("'tolerance' must be a single positive numeric value.", call. = FALSE) }

  # 2. Check GBIF Credentials
  if (is.null(gbif_user))  gbif_user  <- Sys.getenv("GBIF_USER")
  if (is.null(gbif_pwd))   gbif_pwd   <- Sys.getenv("GBIF_PWD")
  if (is.null(gbif_email)) gbif_email <- Sys.getenv("GBIF_EMAIL")

  if (gbif_user == "" || gbif_pwd == "" || gbif_email == "") {
    stop("GBIF credentials not found. Please provide credentials as arguments ",
         "or define GBIF_USER, GBIF_PWD, and GBIF_EMAIL environment variables.",
         call. = FALSE)
  }

  # 3. Handle SpatVector Geometry
  if (is.na(terra::crs(spatial_polygon)) || terra::crs(spatial_polygon) == "") {
    warning("Coordinate reference system missing in 'spatial_polygon'. Assuming EPSG:4326.",
            call. = FALSE)
    terra::crs(spatial_polygon) <- "EPSG:4326"
  } else if (!grepl("4326", terra::crs(spatial_polygon, describe = TRUE)$code %||% "")) {
    spatial_polygon <- terra::project(spatial_polygon, "EPSG:4326")
  }

  # Simplify polygon if vertex count exceeds safe GBIF limits
  if (nrow(terra::crds(spatial_polygon)) > 500) {
    message("Geometry contains >500 vertices. Simplifying geometry...")
    spatial_polygon <- terra::simplifyGeom(spatial_polygon, tolerance = tolerance)
  }

  wkt_geom <- terra::geom(spatial_polygon, wkt = TRUE)
  if (length(wkt_geom) > 1) {
    wkt_geom <- wkt_geom[1]
  }

  # 4. Resolve Taxonomic Backbone Key and specific Predicate Key Name
  tax_ranks <- list(
    kingdom = kingdom, phylum = phylum, class = class,
    order = order, family = family, genus = genus, species = species
  )
  active_ranks <- tax_ranks[!vapply(tax_ranks, is.null, logical(1))]

  pred_list <- list(
    rgbif::pred("geometry", wkt_geom),
    rgbif::pred("hasCoordinate", TRUE),
    rgbif::pred("hasGeospatialIssue", FALSE)
  )

  if (length(active_ranks) > 0) {
    most_specific_rank <- utils::tail(names(active_ranks), 1)
    target_name        <- active_ranks[[most_specific_rank]]

    backbone_res <- rgbif::name_backbone(
      name    = target_name,
      rank    = most_specific_rank,
      kingdom = kingdom,
      phylum  = phylum,
      class   = class,
      order   = order,
      family  = family,
      genus   = genus
    )

    if (backbone_res$matchType == "NONE" || is.null(backbone_res$usageKey)) {
      stop(sprintf("No taxon key match found on GBIF backbone for %s: '%s'",
                   most_specific_rank, target_name), call. = FALSE)
    }

    # Map rank explicitly to rankKey (e.g., family -> familyKey)
    rank_key_param <- if (most_specific_rank == "species") {
      "speciesKey"
    } else {
      paste0(most_specific_rank, "Key")
    }

    # Use the extracted rank-specific key from backbone
    # (falls back to usageKey if rank-specific field is omitted in backbone output)
    target_key <- backbone_res[[rank_key_param]] %||% backbone_res$usageKey

    pred_list <- c(pred_list, list(rgbif::pred(rank_key_param, target_key)))
  }

  combined_predicate <- do.call(rgbif::pred_and, pred_list)

  # 5. Submit Download Request
  if(verbose){
    message("Submitting SPECIES_LIST download request to GBIF...")}
  occ_request <- rgbif::occ_download(
    combined_predicate,
    format = "SPECIES_LIST",
    user   = gbif_user,
    pwd    = gbif_pwd,
    email  = gbif_email
  )

  request_key <- occ_request[1]
  class(request_key) <- "request_key"

  return(request_key)

}
