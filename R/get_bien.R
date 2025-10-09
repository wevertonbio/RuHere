get_bien <- function(by = "species", cultivated = FALSE, new.world = NULL,
                     all.taxonomy = FALSE, native.status = FALSE,
                     natives.only = TRUE, observation.type = FALSE,
                     political.boundaries = TRUE, collection.info = TRUE,
                     only.geovalid = TRUE, min.lat = NULL, max.lat = NULL,
                     min.long = NULL, max.long = NULL, species = NULL,
                     genus = NULL, country = NULL, country.code = NULL,
                     state = NULL, county = NULL, state.code = NULL,
                     county.code = NULL, family = NULL, sf = NULL, ...) {

    # Botanical Information and Ecology Network Database
    # https://bien.nceas.ucsb.edu/bien/

    require(BIEN)

    # Get data type
    if (is.null(by)) {
        stop("Data type must be specified with argument 'by'!")
    }

    if (!is.character(by) || !by %in% c("box", "country", "county", "family",
                                        "genus", "records_per_species",
                                        "species", "sf", "state")) {
        stop("'by' must be one of: box, country, county, family, genus, records_per_species, species, sf, state")
    }

    ##### Not checking other arguments: BIEN already does this!

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

        occ_bien <- BIEN_occurrence_box(
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

    return(occ_bien)
}

# # # Test
# res_test <- get_bien(
#     by = "species",
#     species = "Paubrasilia echinata",
#     cultivated = TRUE,
#     native.status = TRUE,
#     observation.type = TRUE,
#     only.geovalid = T
# )
