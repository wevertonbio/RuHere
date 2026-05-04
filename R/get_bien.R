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
#' save = FALSE, verbose = TRUE, ...)
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
#' @param verbose (logical) if `TRUE`, prints messages about the progress and
#' the number of species being checked. Default is `TRUE`.
#'
#' @return
#' A \code{data.frame} containing BIEN occurrence records that match
#' the specified query. The structure and available columns depend on the chosen
#' `by` value and the corresponding BIEN function.
#'
#' @importFrom data.table fwrite
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Example: download occurrence records for a single species
#' res_test <- get_bien(
#'     by = "species",
#'     species = "Paubrasilia echinata",
#'     cultivated = TRUE,
#'     native.status = TRUE,
#'     observation.type = TRUE,
#'     only.geovalid = TRUE
#' )
#'}
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
                     compress = FALSE, save = FALSE, verbose = TRUE, ...) {

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

    if (!is.logical(verbose) || length(verbose) != 1) {
      stop("'verbose' must be a single logical value (TRUE or FALSE).",
           call. = FALSE)
    }

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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'box': ",
                    paste(invalid_args, collapse = ", "))
        }
    
        taxonomy_select <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""

        native_select <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""

        observation_select <- if (observation.type) ",observation_type" else ""
        observation_query  <- if (observation.type) "" else "AND observation_type IN ('plot','specimen','literature','checklist')"

        political_select <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""

        natives_query <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""

        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""

        species_query <- if (is.null(species)) "" else paste("AND scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"), collapse = ", "), ")")

        genus_query <- if (is.null(genus)) "" else paste("AND scrubbed_genus in (", paste(shQuote(genus, type = "sh"), collapse = ", "), ")")

        query <- paste("SELECT scrubbed_species_binomial", taxonomy_select,
               political_select, native_select,
               ",latitude,longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
               collection_select,
               observation_select,
               "FROM view_full_occurrence_individual",
               "WHERE latitude between", shQuote(min.lat), "AND", shQuote(max.lat),
               "AND longitude between", shQuote(min.long), "AND", shQuote(max.long),
               natives_query, species_query, genus_query, observation_query,
               "AND scrubbed_species_binomial IS NOT NULL ;")

        occ_bien <- BIEN_sql(query, ...)

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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'country': ",
                    paste(invalid_args, collapse = ", "))
        }

        if (is.null(country) & is.null(country.code)) {
            stop("Please supply either a country or 2-digit ISO code")
        }

        taxonomy_select   <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""
        native_select     <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""
        observation_select <- if (observation.type) ",observation_type" else ""
        observation_query  <- if (observation.type) "" else "AND observation_type IN ('plot','specimen','literature','checklist')"
        political_select  <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""
        natives_query     <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""
        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""
        cultivated_select <- if (cultivated) ",is_cultivated_observation,is_cultivated_in_region,is_location_cultivated" else ""
        cultivated_query  <- if (cultivated) "" else "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
        newworld_select   <- if (is.null(new.world)) "" else ", is_new_world"
        newworld_query    <- if (is.null(new.world)) "" else if (new.world) "AND is_new_world = 1" else "AND is_new_world = 0"
        geovalid_select   <- if (only.geovalid) "" else ",is_geovalid"
        geovalid_query    <- if (only.geovalid) "AND is_geovalid = 1" else ""

        if (is.null(country.code)) {
            query <- paste("SELECT scrubbed_species_binomial", taxonomy_select,
                           political_select, native_select,
                           ", latitude, longitude, date_collected, datasource, dataset, dataowner, custodial_institution_codes, collection_code, view_full_occurrence_individual.datasource_id",
                           collection_select, cultivated_select, newworld_select,
                           observation_select, geovalid_select,
                           "FROM view_full_occurrence_individual",
                           "WHERE country in (", paste(shQuote(country, type = "sh"), collapse = ", "), ")",
                           cultivated_query, newworld_query, natives_query, geovalid_query,
                           "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')",
                           "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                           "AND (is_centroid IS NULL OR is_centroid=0)",
                           observation_query, ";")
        } else {
            query <- paste("SELECT scrubbed_species_binomial", taxonomy_select,
                           political_select, native_select,
                           ", latitude, longitude, date_collected, datasource, dataset, dataowner, custodial_institution_codes, collection_code, view_full_occurrence_individual.datasource_id",
                           collection_select, cultivated_select, newworld_select,
                           observation_select,
                           "FROM view_full_occurrence_individual",
                           "WHERE country in (SELECT country FROM country WHERE iso in (",
                           paste(shQuote(country.code, type = "sh"), collapse = ", "), "))",
                           cultivated_query, newworld_query, natives_query,
                           "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1",
                           "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                           "AND (is_centroid IS NULL OR is_centroid=0)",
                           observation_query, "AND scrubbed_species_binomial IS NOT NULL ;")
        }

        occ_bien <- BIEN_sql(query, ...)

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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'county': ",
                    paste(invalid_args, collapse = ", "))
        }

        taxonomy_select   <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""
        native_select     <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""
        observation_select <- if (observation.type) ",observation_type" else ""
        political_select  <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""
        natives_query     <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""
        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""
        cultivated_select <- if (cultivated) ",is_cultivated_observation,is_cultivated_in_region,is_location_cultivated" else ""
        cultivated_query  <- if (cultivated) "" else "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
        newworld_select   <- if (is.null(new.world)) "" else ", is_new_world"
        newworld_query    <- if (is.null(new.world)) "" else if (new.world) "AND is_new_world = 1" else "AND is_new_world = 0"

        if (is.null(country.code) & is.null(state.code) & is.null(county.code)) {
            if (length(country) == 1 & length(state) == 1) {
                sql_where <- paste("WHERE country in (", paste(shQuote(country, type = "sh"), collapse = ", "),
                                   ") AND state_province in (", paste(shQuote(state, type = "sh"), collapse = ", "),
                                   ") AND county in (", paste(shQuote(county, type = "sh"), collapse = ", "),
                                   ") AND scrubbed_species_binomial IS NOT NULL")
            } else {
                if (length(country) == length(state) & length(country) == length(county)) {
                    sql_where <- "WHERE ("
                    for (i in 1:length(country)) {
                        condition_i <- paste("(country =", shQuote(country[i], type = "sh"),
                                             "AND state_province =", shQuote(state[i], type = "sh"),
                                             "AND county =", shQuote(county[i], type = "sh"), ")")
                        if (i != 1) condition_i <- paste("OR", condition_i)
                        sql_where <- paste(sql_where, condition_i)
                    }
                    sql_where <- paste(sql_where, ") AND scrubbed_species_binomial IS NOT NULL")
                } else {
                    stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")
                }
            }
        } else {
            if (length(country.code) == 1 & length(state.code) == 1) {
                sql_where <- paste("WHERE country in (SELECT country FROM country WHERE iso in (",
                                   paste(shQuote(country.code, type = "sh"), collapse = ", "),
                                   ")) AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (",
                                   paste(shQuote(state.code, type = "sh"), collapse = ", "),
                                   ")) AND county in (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (",
                                   paste(shQuote(county.code, type = "sh"), collapse = ", "),
                                   ")) AND scrubbed_species_binomial IS NOT NULL")
            } else {
                if (length(country.code) == length(state.code) & length(country.code) == length(county.code)) {
                    sql_where <- "WHERE ("
                    for (i in 1:length(country.code)) {
                        condition_i <- paste("(country = (SELECT country FROM country WHERE iso in (", shQuote(country.code[i], type = "sh"),
                                             ")) AND state_province = (SELECT state_province_ascii FROM county_parish WHERE admin1code in (", shQuote(state.code[i], type = "sh"),
                                             ")) AND county = (SELECT county_parish_ascii FROM county_parish WHERE admin2code in (", shQuote(county.code[i], type = "sh"), ")))")
                        if (i != 1) condition_i <- paste("OR", condition_i)
                        sql_where <- paste(sql_where, condition_i)
                    }
                    sql_where <- paste(sql_where, ") AND scrubbed_species_binomial IS NOT NULL")
                } else {
                    stop("If supplying more than one country and/or state the function requires matching vectors of countries, states and counties.")
                }
            }
        }

        query <- paste("SELECT scrubbed_species_binomial", taxonomy_select,
                       political_select,
                       ",latitude,longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                       collection_select, cultivated_select, newworld_select,
                       native_select, observation_select,
                       "FROM view_full_occurrence_individual",
                       sql_where, cultivated_query, newworld_query, natives_query,
                       "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1",
                       "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                       "AND (is_centroid IS NULL OR is_centroid=0)",
                       "AND observation_type IN ('plot','specimen','literature','checklist')",
                       "AND scrubbed_species_binomial IS NOT NULL ;")

        occ_bien <- BIEN_sql(query, ...)

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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'family': ",
                    paste(invalid_args, collapse = ", "))
        }

        taxonomy_select   <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""
        native_select     <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""
        observation_select <- if (observation.type) ",observation_type" else ""
        observation_query  <- if (observation.type) "" else "AND observation_type IN ('plot','specimen','literature','checklist')"
        political_select  <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""
        natives_query     <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""
        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""
        cultivated_select <- if (cultivated) ",is_cultivated_observation,is_cultivated_in_region,is_location_cultivated" else ""
        cultivated_query  <- if (cultivated) "" else "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
        newworld_select   <- if (is.null(new.world)) "" else ", is_new_world"
        newworld_query    <- if (is.null(new.world)) "" else if (new.world) "AND is_new_world = 1" else "AND is_new_world = 0"
        geovalid_select   <- if (only.geovalid) "" else ",is_geovalid"
        geovalid_query    <- if (only.geovalid) "AND is_geovalid = 1" else ""

        query <- paste("SELECT scrubbed_family", taxonomy_select,
                       native_select, political_select,
                       ", scrubbed_species_binomial, latitude, longitude, date_collected, datasource, dataset, dataowner, custodial_institution_codes, collection_code, view_full_occurrence_individual.datasource_id",
                       collection_select, cultivated_select, newworld_select,
                       observation_select, geovalid_select,
                       "FROM view_full_occurrence_individual",
                       "WHERE scrubbed_family in (", paste(shQuote(family, type = "sh"), collapse = ", "), ")",
                       cultivated_query, newworld_query, natives_query,
                       observation_query, geovalid_query,
                       "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')",
                       "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                       "AND (is_centroid IS NULL OR is_centroid=0)",
                       "AND scrubbed_species_binomial IS NOT NULL ;")

        occ_bien <- BIEN_sql(query, ...)

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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'genus': ",
                    paste(invalid_args, collapse = ", "))
        }

        taxonomy_select   <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""
        native_select     <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""
        observation_select <- if (observation.type) ",observation_type" else ""
        political_select  <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""
        natives_query     <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""
        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""
        cultivated_select <- if (cultivated) ",is_cultivated_observation,is_cultivated_in_region,is_location_cultivated" else ""
        cultivated_query  <- if (cultivated) "" else "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
        newworld_select   <- if (is.null(new.world)) "" else ", is_new_world"
        newworld_query    <- if (is.null(new.world)) "" else if (new.world) "AND is_new_world = 1" else "AND is_new_world = 0"

        query <- paste("SELECT scrubbed_genus, scrubbed_species_binomial",
                       taxonomy_select, native_select, political_select,
                       ",latitude,longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                       collection_select, cultivated_select, newworld_select,
                       observation_select,
                       "FROM view_full_occurrence_individual",
                       "WHERE scrubbed_genus in (", paste(shQuote(genus, type = "sh"), collapse = ", "), ")",
                       cultivated_query, newworld_query, natives_query,
                       "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1",
                       "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                       "AND (is_centroid IS NULL OR is_centroid=0)",
                       "AND observation_type IN ('plot','specimen','literature','checklist')",
                       "AND scrubbed_species_binomial IS NOT NULL ;")

        occ_bien <- BIEN_sql(query, ...)

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
        if (political.boundaries == FALSE) {
            invalid_args <- c(invalid_args, "political.boundaries")
        }
        if (collection.info == FALSE) {
            invalid_args <- c(invalid_args, "collection.info")
        }
        if (only.geovalid == FALSE) {
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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'records_per_species': ",
                    paste(invalid_args, collapse = ", "))
        }

        if (is.null(species)) {
            query <- "SELECT DISTINCT scrubbed_species_binomial, count(*)
                      FROM view_full_occurrence_individual
                      WHERE is_geovalid = 1
                      AND latitude IS NOT NULL
                      AND LONGITUDE IS NOT NULL
                      GROUP BY scrubbed_species_binomial ;"
        } else {
            query <- paste("SELECT scrubbed_species_binomial, count(*)",
                           "FROM view_full_occurrence_individual",
                           "WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"), collapse = ", "), ")",
                           "AND is_geovalid = 1",
                           "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                           "AND (is_centroid IS NULL OR is_centroid=0)",
                           "AND observation_type IN ('plot','specimen','literature','checklist')",
                           "GROUP BY scrubbed_species_binomial ;")
        }

        occ_bien <- BIEN_sql(query, ...)

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

        taxonomy_select   <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""
        native_select     <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""
        observation_select <- if (observation.type) ",observation_type" else ""
        observation_query  <- if (observation.type) "" else "AND observation_type IN ('plot','specimen','literature','checklist')"
        political_select  <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""
        natives_query     <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""
        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""
        cultivated_select <- if (cultivated) ",is_cultivated_observation,is_cultivated_in_region,is_location_cultivated" else ""
        cultivated_query  <- if (cultivated) "" else "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
        newworld_select   <- if (is.null(new.world)) "" else ", is_new_world"
        newworld_query    <- if (is.null(new.world)) "" else if (new.world) "AND is_new_world = 1" else "AND is_new_world = 0"
        geovalid_select   <- if (only.geovalid) "" else ",is_geovalid"
        geovalid_query    <- if (only.geovalid) "AND is_geovalid = 1" else ""

        query <- paste("SELECT scrubbed_species_binomial",
                       taxonomy_select, native_select, political_select,
                       ",latitude,longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                       collection_select, cultivated_select, newworld_select,
                       observation_select, geovalid_select,
                       "FROM view_full_occurrence_individual",
                       "WHERE scrubbed_species_binomial in (", paste(shQuote(species, type = "sh"), collapse = ", "), ")",
                       cultivated_query, newworld_query, natives_query,
                       observation_query, geovalid_query,
                       "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')",
                       "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                       "AND (is_centroid IS NULL OR is_centroid=0)",
                       "AND scrubbed_species_binomial IS NOT NULL ;")

        occ_bien <- BIEN_sql(query, ...)

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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'sf': ",
                    paste(invalid_args, collapse = ", "))
        }

        taxonomy_select   <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""
        native_select     <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""
        observation_select <- if (observation.type) ",observation_type" else ""
        observation_query  <- if (observation.type) "" else "AND observation_type IN ('plot','specimen','literature','checklist')"
        political_select  <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""
        natives_query     <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""
        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""
        cultivated_select <- if (cultivated) ",is_cultivated_observation,is_cultivated_in_region,is_location_cultivated" else ""
        cultivated_query  <- if (cultivated) "" else "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
        newworld_select   <- if (is.null(new.world)) "" else ", is_new_world"
        newworld_query    <- if (is.null(new.world)) "" else if (new.world) "AND is_new_world = 1" else "AND is_new_world = 0"

        wkt      <- sf::st_as_text(sf::st_geometry(sf))
        sf_bbox  <- sf::st_bbox(sf)
        lat_min  <- sf_bbox["ymin"]
        lat_max  <- sf_bbox["ymax"]
        long_min <- sf_bbox["xmin"]
        long_max <- sf_bbox["xmax"]

        query <- paste("SELECT scrubbed_species_binomial", taxonomy_select,
                       native_select, political_select,
                       ",latitude,longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,a.datasource_id",
                       collection_select, cultivated_select, newworld_select,
                       observation_select,
                       "FROM (SELECT * FROM view_full_occurrence_individual",
                       "WHERE higher_plant_group NOT IN ('Algae','Bacteria','Fungi')",
                       "AND is_geovalid = 1",
                       "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                       "AND (is_centroid IS NULL OR is_centroid=0)",
                       "AND latitude BETWEEN", lat_min, "AND", lat_max,
                       "AND longitude BETWEEN", long_min, "AND", long_max, ") a",
                       "WHERE st_intersects(ST_GeographyFromText('SRID=4326;",
                       paste(wkt), "'),a.geom)",
                       cultivated_query, newworld_query, natives_query,
                       observation_query,
                       "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi')",
                       "AND is_geovalid = 1",
                       "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                       "AND (is_centroid IS NULL OR is_centroid=0)",
                       "AND scrubbed_species_binomial IS NOT NULL ;")

        occ_bien <- BIEN_sql(query, ...)
        
        if (length(occ_bien) == 0) {
            message("No occurrences found")
            return(invisible(NULL))
        }

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

        if (length(invalid_args) > 0 && verbose) {
            warning("The following arguments are not valid for 'state': ",
                    paste(invalid_args, collapse = ", "))
        }

        taxonomy_select   <- if (all.taxonomy) ", verbatim_family,verbatim_scientific_name,family_matched,name_matched,name_matched_author,higher_plant_group,scrubbed_taxonomic_status,scrubbed_family,scrubbed_author" else ""
        native_select     <- if (native.status) ",native_status,native_status_reason,native_status_sources,is_introduced,native_status_country,native_status_state_province,native_status_county_parish" else ""
        observation_select <- if (observation.type) ",observation_type" else ""
        political_select  <- if (political.boundaries) ", country,state_province,county,locality,elevation_m" else ""
        natives_query     <- if (natives.only) "AND (is_introduced=0 OR is_introduced IS NULL)" else ""
        collection_select <- if (collection.info) ",catalog_number,recorded_by,record_number,date_collected,identified_by,date_identified,identification_remarks" else ""
        cultivated_select <- if (cultivated) ",is_cultivated_observation,is_cultivated_in_region,is_location_cultivated" else ""
        cultivated_query  <- if (cultivated) "" else "AND (is_cultivated_observation = 0 OR is_cultivated_observation IS NULL) AND is_location_cultivated IS NULL"
        newworld_select   <- if (is.null(new.world)) "" else ", is_new_world"
        newworld_query    <- if (is.null(new.world)) "" else if (new.world) "AND is_new_world = 1" else "AND is_new_world = 0"

        if (is.null(country.code) & is.null(state.code)) {
            if (length(country) == 1) {
                sql_where <- paste("WHERE country in (", paste(shQuote(country, type = "sh"), collapse = ", "),
                                   ") AND state_province in (", paste(shQuote(state, type = "sh"), collapse = ", "),
                                   ") AND scrubbed_species_binomial IS NOT NULL")
            } else {
                if (length(country) == length(state)) {
                    sql_where <- "WHERE ("
                    for (i in 1:length(country)) {
                        condition_i <- paste("(country =", shQuote(country[i], type = "sh"),
                                             "AND state_province =", shQuote(state[i], type = "sh"), ")")
                        if (i != 1) condition_i <- paste("OR", condition_i)
                        sql_where <- paste(sql_where, condition_i)
                    }
                    sql_where <- paste(sql_where, ") AND scrubbed_species_binomial IS NOT NULL")
                } else {
                    stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")
                }
            }
        } else {
            if (length(country.code) == 1) {
                sql_where <- paste("WHERE country in (SELECT country FROM country WHERE iso in (",
                                   paste(shQuote(country.code, type = "sh"), collapse = ", "),
                                   ")) AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (",
                                   paste(shQuote(state.code, type = "sh"), collapse = ", "),
                                   ")) AND scrubbed_species_binomial IS NOT NULL")
            } else {
                if (length(country.code) == length(state.code)) {
                    sql_where <- "WHERE ("
                    for (i in 1:length(country.code)) {
                        condition_i <- paste("country in (SELECT country FROM country WHERE iso in (",
                                             shQuote(country.code[i], type = "sh"),
                                             ")) AND state_province in (SELECT state_province_ascii FROM county_parish WHERE admin1code in (",
                                             shQuote(state.code[i], type = "sh"), "))")
                        if (i != 1) condition_i <- paste("OR", condition_i)
                        sql_where <- paste(sql_where, condition_i)
                    }
                    sql_where <- paste(sql_where, ") AND scrubbed_species_binomial IS NOT NULL")
                } else {
                    stop("If supplying more than one country, the function requires a vector of countries corresponding to the vector of states")
                }
            }
        }

        query <- paste("SELECT scrubbed_species_binomial", taxonomy_select,
                       political_select,
                       ",latitude,longitude,date_collected,datasource,dataset,dataowner,custodial_institution_codes,collection_code,view_full_occurrence_individual.datasource_id",
                       collection_select, cultivated_select, newworld_select,
                       native_select, observation_select,
                       "FROM view_full_occurrence_individual",
                       sql_where, cultivated_query, newworld_query, natives_query,
                       "AND higher_plant_group NOT IN ('Algae','Bacteria','Fungi') AND is_geovalid = 1",
                       "AND (georef_protocol is NULL OR georef_protocol<>'county centroid')",
                       "AND (is_centroid IS NULL OR is_centroid=0)",
                       "AND observation_type IN ('plot','specimen','literature','checklist')",
                       "AND scrubbed_species_binomial IS NOT NULL ;")

        occ_bien <- BIEN_sql(query, ...)
    }

    if (save) {

      if (file.format == "csv") {
        if (compress) {
          fullname <- file.path(dir, paste0(filename, ".csv.zip"))
          if (verbose) message(paste0("Writing ", fullname, " on disk."))
          data.table::fwrite(occ_bien, file = fullname, compress = "gzip")
        }
        else {
          fullname <- file.path(dir, paste0(filename, ".csv"))
          if (verbose) message(paste0("Writing ", fullname, " on disk."))
          data.table::fwrite(occ_bien, file = fullname)
        }
      }

      if (file.format == "rds") {
        fullname <- file.path(dir, paste0(filename, ".rds"))
        if (verbose) message(paste0("Writing ", fullname, " on disk."))
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
