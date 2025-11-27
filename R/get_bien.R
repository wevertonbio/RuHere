#' Download occurrence records from BIEN
#'
#' @usage get_bien(by = "species", cultivated = FALSE,
#' new.world = NULL, all.taxonomy = FALSE, native.status = FALSE,
#' natives.only = TRUE, observation.type = FALSE, political.boundaries = TRUE,
#' collection.info = TRUE, only.geovalid = TRUE, min.lat = NULL, max.lat = NULL,
#' min.long = NULL, max.long = NULL, species = NULL, genus = NULL,
#' country = NULL, country.code = NULL, state = NULL, county = NULL,
#' state.code = NULL, county.code = NULL, family = NULL, sf = NULL, dir,
#' filename = "bien_output", file.format = "csv", compress = FALSE,
#' save = FALSE, ...)
#'
#' @description
#' Wrapper function to access and download occurrence records from the
#' Botanical Information and Ecology Network (BIEN) database. It provides a
#' unified interface to query BIEN data by species, genus, family, or by
#' geographic or political boundaries.
#'
#' @param by (character) type of query to perform (`"box"`, `"country"`,
#' `"county"`, `"family"`, `"genus"`, `"records_per_species"`, `"species"`,
#' `"sf"`, or `"state"`). Default is `species`.
#' @param cultivated (logical) whether to include cultivated records or exclude
#' them. Default is `FALSE`.
#' @param new.world (logical) if `TRUE`, restricts records to the New World,
#' if `FALSE`, to the Old World, and if `NULL`, no restriction. Default is `NULL`.
#' @param all.taxonomy (logical) if `TRUE`, returns all taxonomic levels
#' available, otherwise, limits results to accepted names. Default is `FALSE`.
#' @param native.status (logical) if `TRUE`, includes information about native
#' versus non-native status of occurrences. Default is `FALSE`.
#' @param natives.only (logical) if `TRUE`, restricts results to native species
#' only. Default is `TRUE`.
#' @param observation.type (logical) if `TRUE`, includes information on
#' observation types. Default is `FALSE`.
#' @param political.boundaries (logical) if `TRUE`, restricts the search to
#' defined political boundaries. Default is `TRUE`.
#' @param collection.info (logical) if `TRUE`, includes collection-level
#' metadata. Default is `TRUE`.
#' @param only.geovalid (logical) if `TRUE`, restricts output to
#' georeferenced and spatially valid records. Default is `TRUE`.
#' @param min.lat (numeric) the minimum latitude (in decimal degrees) for a
#' bounding-box query when `by = "box"`.
#' @param max.lat (numeric) the maximum latitude (in decimal degrees) for a
#' bounding-box query when `by = "box"`.
#' @param min.long (numeric) the minimum longitude (in decimal degrees) for a
#' bounding-box query when `by = "box"`.
#' @param max.long (numeric) the maximum longitude (in decimal degrees) for a
#' bounding-box query when `by = "box"`.
#' Ignored otherwise. Default is `NULL`.
#' @param species (character) species name(s) to query when `by = "species"`
#' or `"records_per_species"`. Default is `NULL`.
#' @param genus (character) genus name(s) to query when `by = "genus"`. Default
#' is `NULL`.
#' @param family (character) family name(s) to query when `by = "family"`.
#' Default is `NULL`.
#' @param country (character) country name when `by = "country"`, `"state"`,
#' or `"county"`. Default is `NULL`.
#' @param country.code (character) two-letter ISO country code corresponding
#' to `country`. Default is `NULL`.
#' @param state (character) state or province name when `by = "state"` or
#' `"county"`. Default is `NULL`.
#' @param state.code (character) state or province code corresponding
#' to `state`. Default is `NULL`.
#' @param county (character) county or equivalent subdivision name
#' when `by = "county"`. Default is `NULL`.
#' @param county.code (character) county or equivalent subdivision code
#' corresponding to `county`. Default is `NULL`.
#' @param sf (object of class `sf`) a spatial object defining an area
#' of interest when `by = "sf"`.  Default is `NULL`.
#' @param dir (character) directory path where the file will be saved.
#' Required if `save = TRUE`.
#' @param filename (character) name of the output file without extension.
#' Default is `"bien_output"`.
#' @param save (logical) if `TRUE`, saves the results to a CSV file.
#' Default is `FALSE`.
#' @param file.format (character) file format for saving output (`"csv"`,
#' `"rds"`).
#' Default is `"csv"`.
#' @param compress (logical) if `TRUE` and `save = TRUE`, compresses the output
#' file as .csv.zip. Default is `FALSE`.
#' @param ... additional arguments passed to the underlying BIEN function.
#'
#' @return
#' A \code{data.frame} containing BIEN occurrence records that match
#' the specified query. The structure and available columns depend on the chosen
#' `by` value and the corresponding BIEN function.
#'
#' @importFrom BIEN BIEN_occurrence_box BIEN_occurrence_country
#' @importFrom BIEN BIEN_occurrence_county BIEN_occurrence_family
#' @importFrom BIEN BIEN_occurrence_genus BIEN_occurrence_records_per_species
#' @importFrom BIEN BIEN_occurrence_species BIEN_occurrence_sf
#' @importFrom BIEN BIEN_occurrence_state
#' @importFrom data.table fwrite
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example: download occurrence records for a single species
#' res_test <- get_bien(
#'     by = "species",
#'     species = "Paubrasilia echinata",
#'     cultivated = TRUE,
#'     native.status = TRUE,
#'     observation.type = TRUE,
#'     only.geovalid = TRUE
#' )
#' }
#'
get_bien <- function(by = "species", cultivated = FALSE, new.world = NULL,
                     all.taxonomy = FALSE, native.status = FALSE,
                     natives.only = TRUE, observation.type = FALSE,
                     political.boundaries = TRUE, collection.info = TRUE,
                     only.geovalid = TRUE, min.lat = NULL, max.lat = NULL,
                     min.long = NULL, max.long = NULL, species = NULL,
                     genus = NULL, country = NULL, country.code = NULL,
                     state = NULL, county = NULL, state.code = NULL,
                     county.code = NULL, family = NULL, sf = NULL, dir,
                     filename = "bien_output", file.format = "csv",
                     compress = FALSE, save = FALSE, ...) {

    # Botanical Information and Ecology Network Database
    # https://bien.nceas.ucsb.edu/bien/

    # require(BIEN) # Não colocar dentro da função

    # Get data type
    if (is.null(by)) {
        stop("Data type must be specified with argument 'by'!")
    }

    valid_by <- c("box", "country", "county", "family", "genus",
                "records_per_species", "species", "sf", "state")
    if (!inherits(by, "character") || length(by) != 1 || !(by %in% valid_by)) {
      stop("'by' must be one of: ", paste(valid_by, collapse = ", "),
           call. = FALSE)
    }

    # Logical arguments
    logical_args <- list(
      cultivated = cultivated,
      all.taxonomy = all.taxonomy,
      native.status = native.status,
      natives.only = natives.only,
      observation.type = observation.type,
      political.boundaries = political.boundaries,
      collection.info = collection.info,
      only.geovalid = only.geovalid
    )

    for (arg in names(logical_args)) {
      val <- logical_args[[arg]]
      if (!inherits(val, "logical") || length(val) != 1) {
        stop("'", arg, "' must be a single logical value (TRUE or FALSE).",
             call. = FALSE)
      }
    }

    if (!is.null(new.world) && !inherits(new.world, "logical")) {
      stop("'new.world' must be a single logical value (TRUE or FALSE) or NULL.",
           call. = FALSE)
    }

    text_args <- c("country", "country.code", "state", "state.code", "county",
                   "county.code")
    for (nm in text_args) {
      val <- get(nm)
      if (!is.null(val) && !inherits(val, "character")) {
        stop("'", nm, "' must be a character or NULL, not ", paste(class(val),
                                                                   collapse = "/"))
      }
    }

    num_args <- c("min.lat", "max.lat", "min.long", "max.long")
    for (nm in num_args) {
      val <- get(nm)
      if (!is.null(val) && !inherits(val, "numeric")) {
        stop("'", nm, "' must be numeric or NULL, not ", paste(class(val),
                                                               collapse = "/"))
      }
    }

    # Taxonomic fields
    if (!is.null(species) && !inherits(species, "character"))
      stop("'species' must be a character vector or NULL, not ", class(species),
           call. = FALSE)
    if (!is.null(genus) && !inherits(genus, "character"))
      stop("'genus' must be a character vector or NULL, not ", class(genus),
           call. = FALSE)
    if (!is.null(family) && !inherits(family, "character"))
      stop("'family' must be a character vector or NULL, not ", class(family),
           call. = FALSE)

    # Spatial object
    if (!is.null(sf)) {
      if (!inherits(sf, "sf")) {
        stop("'sf' must be an object of class 'sf' or NULL, not ", class(sf),
             call. = FALSE)
      }
    }

    if (!inherits(filename, "character") || length(filename) != 1)
      stop("'filename' must be a single character value, not ",
           paste(class(filename), collapse = "/"))

    if (!inherits(file.format, "character") || length(file.format) != 1)
      stop("'file.format' must be a single character ('csv' or 'rds'), not ",
           paste(class(file.format), collapse = "/"))

    if (!file.format %in% c("csv", "rds"))
      stop("'file.format' must be either 'csv' or 'rds'")

    if (!inherits(save, "logical") || length(save) != 1)
      stop("'save' must be a single logical (TRUE/FALSE), not ",
           paste(class(save), collapse = "/"))

    if (isTRUE(save)) {
      if (missing(dir) || is.null(dir)) {
        stop("'dir' is required (must not be NULL or missing) when save = TRUE.")
      }
      if (!inherits(dir, "character") || length(dir) != 1) {
        stop("'dir' must be a single character string when save = TRUE, not ",
             class(dir))
      }
      if (!dir.exists(dir)) {
        stop(paste0("Directory '", dir, "' does not exist. It must be created before saving."))
      }
    }

    if (!inherits(compress, "logical") || length(compress) != 1)
      stop("'compress' must be a single logical (TRUE/FALSE), not ",
           paste(class(compress), collapse = "/"))

    if (by == "box") {

        invalid_args <- c()
        if (only.geovalid == TRUE) invalid_args <-
            c(invalid_args, "only.geovalid")
        if (!is.null(country)) invalid_args <- c(invalid_args, "country")
        if (!is.null(country.code)) invalid_args <- c(invalid_args,
                                                      "country.code")
        if (!is.null(state)) invalid_args <- c(invalid_args, "state")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(state.code)) invalid_args <- c(invalid_args, "state.code")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'box': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_box(
            min.lat = min.lat,
            max.lat = max.lat,
            min.long = min.long,
            max.long = max.long,
            species = species,
            genus = genus,
            cultivated = cultivated,
            new.world = new.world,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            observation.type = observation.type,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            ...
        )
    } else if (by == "country") {

        invalid_args <- c()
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(species)) invalid_args <- c(invalid_args, "species")
        if (!is.null(genus)) invalid_args <- c(invalid_args, "genus")
        if (!is.null(state)) invalid_args <- c(invalid_args, "state")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(state.code)) invalid_args <- c(invalid_args, "state.code")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'country': ",
                    paste(invalid_args, collapse = ", "))
        }

        if (is.null(country) & is.null(country.code)) {
            stop("Please supply either a country or 2-digit ISO code")
        }

        occ_bien <- BIEN::BIEN_occurrence_country(
            country = country,
            country.code = country.code,
            cultivated = cultivated,
            new.world = new.world,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            observation.type = observation.type,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            only.geovalid = only.geovalid,
            ...
        )
    } else if (by == "county") {

        invalid_args <- c()
        if (only.geovalid == TRUE) invalid_args <-
            c(invalid_args, "only.geovalid")
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(species)) invalid_args <- c(invalid_args, "species")
        if (!is.null(genus)) invalid_args <- c(invalid_args, "genus")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'county': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_county(
            country = country,
            state = state,
            county = county,
            country.code = country.code,
            state.code = state.code,
            county.code = county.code,
            cultivated = cultivated,
            new.world = new.world,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            observation.type = observation.type,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            ...
        )
    } else if (by == "family") {

        invalid_args <- c()
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(species)) invalid_args <- c(invalid_args, "species")
        if (!is.null(genus)) invalid_args <- c(invalid_args, "genus")
        if (!is.null(country)) invalid_args <- c(invalid_args, "country")
        if (!is.null(country.code)) invalid_args <- c(invalid_args,
                                                      "country.code")
        if (!is.null(state)) invalid_args <- c(invalid_args, "state")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(state.code)) invalid_args <- c(invalid_args, "state.code")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'family': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_family(
            family = family,
            cultivated = cultivated,
            new.world = new.world,
            observation.type = observation.type,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            only.geovalid = only.geovalid,
            ...
        )
    } else if (by == "genus") {

        invalid_args <- c()
        if (only.geovalid == TRUE) invalid_args <-
            c(invalid_args, "only.geovalid")
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(species)) invalid_args <- c(invalid_args, "species")
        if (!is.null(country)) invalid_args <- c(invalid_args, "country")
        if (!is.null(country.code)) invalid_args <- c(invalid_args,
                                                      "country.code")
        if (!is.null(state)) invalid_args <- c(invalid_args, "state")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(state.code)) invalid_args <- c(invalid_args, "state.code")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'genus': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_genus(
            genus = genus,
            cultivated = cultivated,
            new.world = new.world,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            observation.type = observation.type,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            ...
        )
    } else if (by == "records_per_species") {

        invalid_args <- c()
        if (cultivated == TRUE) invalid_args <- c(invalid_args, "cultivated")
        if (!is.null(new.world)) invalid_args <- c(invalid_args, "new.world")
        if (all.taxonomy == TRUE) invalid_args <- c(invalid_args,
                                                    "all.taxonomy")
        if (native.status == TRUE) invalid_args <- c(invalid_args,
                                                     "native.status")
        if (natives.only == FALSE) invalid_args <- c(invalid_args,
                                                    "natives.only")
        if (observation.type == TRUE) {
            invalid_args <- c(invalid_args, "observation.type")
        }
        if (political.boundaries == TRUE) {
            invalid_args <- c(invalid_args, "political.boundaries")
        }
        if (collection.info == TRUE) {
            invalid_args <- c(invalid_args, "collection.info")
        }
        if (only.geovalid == TRUE) {
            invalid_args <- c(invalid_args, "only.geovalid")
        }
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(genus)) invalid_args <- c(invalid_args, "genus")
        if (!is.null(country)) invalid_args <- c(invalid_args, "country")
        if (!is.null(country.code)) invalid_args <- c(invalid_args,
                                                      "country.code")
        if (!is.null(state)) invalid_args <- c(invalid_args, "state")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(state.code)) invalid_args <- c(invalid_args, "state.code")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'records_per_species': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_records_per_species(
            species = species,
            ...
        )
    } else if (by == "species") {

        invalid_args <- c()
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(genus)) invalid_args <- c(invalid_args, "genus")
        if (!is.null(country)) invalid_args <- c(invalid_args, "country")
        if (!is.null(country.code)) invalid_args <- c(invalid_args,
                                                      "country.code")
        if (!is.null(state)) invalid_args <- c(invalid_args, "state")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(state.code)) invalid_args <- c(invalid_args, "state.code")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'species': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_species(
            species = species,
            cultivated = cultivated,
            new.world = new.world,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            observation.type = observation.type,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            only.geovalid = only.geovalid,
            ...
        )
    } else if (by == "sf") {

        invalid_args <- c()
        if (only.geovalid == TRUE) invalid_args <-
            c(invalid_args, "only.geovalid")
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(genus)) invalid_args <- c(invalid_args, "genus")
        if (!is.null(species)) invalid_args <- c(invalid_args, "species")
        if (!is.null(country)) invalid_args <- c(invalid_args, "country")
        if (!is.null(country.code)) invalid_args <- c(invalid_args,
                                                      "country.code")
        if (!is.null(state)) invalid_args <- c(invalid_args, "state")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(state.code)) invalid_args <- c(invalid_args, "state.code")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'sf': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_sf(
            sf = sf,
            cultivated = cultivated,
            new.world = new.world,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            observation.type = observation.type,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            ...
        )
    } else if (by == "state") {

        invalid_args <- c()
        if (only.geovalid == TRUE) invalid_args <-
            c(invalid_args, "only.geovalid")
        if (!is.null(min.lat)) invalid_args <- c(invalid_args, "min.lat")
        if (!is.null(max.lat)) invalid_args <- c(invalid_args, "max.lat")
        if (!is.null(min.long)) invalid_args <- c(invalid_args, "min.long")
        if (!is.null(max.long)) invalid_args <- c(invalid_args, "max.long")
        if (!is.null(genus)) invalid_args <- c(invalid_args, "genus")
        if (!is.null(species)) invalid_args <- c(invalid_args, "species")
        if (!is.null(county)) invalid_args <- c(invalid_args, "county")
        if (!is.null(county.code)) invalid_args <- c(invalid_args,
                                                     "county.code")
        if (!is.null(family)) invalid_args <- c(invalid_args, "family")
        if (!is.null(sf)) invalid_args <- c(invalid_args, "sf")

        if (length(invalid_args) > 0) {
            warning("The following arguments are not valid for 'state': ",
                    paste(invalid_args, collapse = ", "))
        }

        occ_bien <- BIEN::BIEN_occurrence_state(
            country = country,
            state = state,
            country.code = country.code,
            state.code = state.code,
            cultivated = cultivated,
            new.world = new.world,
            all.taxonomy = all.taxonomy,
            native.status = native.status,
            natives.only = natives.only,
            observation.type = observation.type,
            political.boundaries = political.boundaries,
            collection.info = collection.info,
            ...
        )
    }

    if (save) {

      if (file.format == "csv") {
        if (compress) {
          fullname <- file.path(dir, paste0(filename, ".csv.zip"))
          message(paste0("Writing ", fullname, " on disk."))
          data.table::fwrite(occ_bien, file = fullname, compress = "gzip")
        }
        else {
          fullname <- file.path(dir, paste0(filename, ".csv"))
          message(paste0("Writing ", fullname, " on disk."))
          data.table::fwrite(occ_bien, file = fullname)
        }
      }

      if (file.format == "rds") {
        fullname <- file.path(dir, paste0(filename, ".rds"))
        message(paste0("Writing ", fullname, " on disk."))
        if (compress) {
          saveRDS(occ_bien, file = fullname, compress = "gzip")
        }
        else {
          saveRDS(occ_bien, file = fullname, compress = FALSE)
        }
      }

    }

    return(occ_bien)
}
