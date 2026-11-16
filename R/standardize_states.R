#' Standardize state names
#'
#' @description
#' This function standardizes state names using both names and codes present
#' in a specified column.
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records,
#' preferably standardized using `format_columns()`.
#' @param state_column (character) the column name containing the state
#' information. Default is `"stateProvince"`.
#' @param country_column (character) the column name containing the country
#' information. Default is `"country_suggested"`.
#' @param max_distance (numeric) maximum allowed distance (as a fraction) when
#' searching for suggestions for misspelled state names. Can be any value
#' between 0 and 1. Higher values return more suggestions. See `agrep()` for
#' details. Default is `0.1`.
#' @param lookup_na_state (logical) whether to extract the state from
#' coordinates when the state column has missing values or ambiguous fuzzy
#' matches. If `TRUE`, longitude and latitude columns must be provided. Default
#' is `FALSE`.
#' @param long (character) column name with longitude. Only applicable if
#' `lookup_na_state = TRUE`. Default is `"decimalLongitude"`.
#' @param lat (character) column name with latitude. Only applicable if
#' `lookup_na_state = TRUE`. Default is `"decimalLatitude"`.
#' @param return_dictionary (logical) whether to return the dictionary of
#' states that were (fuzzy) matched. Default is `TRUE`.
#'
#' @details
#' State names are first standardized by exact matching against a list of
#' state names in several languages from `rnaturalearthdata::states50`.
#' Any unmatched names are then processed using a fuzzy matching algorithm to
#' find potential candidates for misspelled state names. When fuzzy matching
#' yields multiple candidate states for a single `(state, country)` pair, the
#' ambiguity is resolved using geographic coordinates if `lookup_na_state = TRUE`
#' (retaining the candidate that matches the point's location), or set to `NA`
#' when coordinates are unavailable or `lookup_na_state = FALSE`. If unmatched
#' names remain and `lookup_na_state = TRUE`, the state is extracted from
#' coordinates using a map retrieved from `rnaturalearthdata::states50`.
#'
#' @returns
#' If `return_dictionary = FALSE`, returns the original `occ` `data.frame`
#' augmented with a `state_suggested` column (and `state_source` if
#' `lookup_na_state = TRUE`).
#' If `return_dictionary = TRUE`, returns a list with two elements:
#' \item{occ}{The original `occ` data.frame with the `state_suggested` column.}
#' \item{report}{A data.frame with the original state names and the suggested
#' matches.}
#'
#' @importFrom florabr match_names
#'
#' @export
#'
#' @examples
#' # Import and standardize GBIF
#' data("occ_gbif", package = "RuHere")
#' gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
#' # Import and standardize SpeciesLink
#' data("occ_splink", package = "RuHere")
#' splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
#' # Import and standardize BIEN
#' data("occ_bien", package = "RuHere")
#' bien_standardized <- format_columns(occ_bien, metadata = "bien")
#' # Import and standardize idigbio
#' data("occ_idig", package = "RuHere")
#' idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
#' # Merge all
#' all_occ <- bind_here(gbif_standardized, splink_standardized,
#'                      bien_standardized, idig_standardized)
#' # Standardize countries
#' occ_standardized <- standardize_countries(occ = all_occ)
#' # Standardize states
#' occ_standardized2 <- standardize_states(occ = occ_standardized$occ)
standardize_states <- function(occ,
                               state_column = "stateProvince",
                               country_column = "country_suggested",
                               max_distance = 0.1,
                               lookup_na_state = FALSE,
                               long = "decimalLongitude",
                               lat = "decimalLatitude",
                               return_dictionary = TRUE) {
  # ---- ARGUMENT CHECKING ----

  # 1. Check occ
  if (missing(occ) || !inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame containing occurrence records.", call. = FALSE)
  }
  if (nrow(occ) == 0L) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.", call. = FALSE)
  }

  # 2. Check state_column
  if (!inherits(state_column, "character") || length(state_column) != 1L || is.na(state_column)) {
    stop("'state_column' must be a single character string specifying the column with state information.", call. = FALSE)
  }
  if (!state_column %in% names(occ)) {
    stop(paste0("The column specified in 'state_column' ('", state_column, "') was not found in 'occ'."), call. = FALSE)
  }

  # 3. Check country_column
  if (!inherits(country_column, "character") || length(country_column) != 1L || is.na(country_column)) {
    stop("'country_column' must be a single character string specifying the column with country information.", call. = FALSE)
  }
  if (!country_column %in% names(occ)) {
    stop(paste0("The column specified in 'country_column' ('", country_column, "') was not found in 'occ'."), call. = FALSE)
  }

  # 4. Check max_distance
  if (!is.numeric(max_distance) || length(max_distance) != 1L || is.na(max_distance) || max_distance < 0 || max_distance > 1) {
    stop("'max_distance' must be a numeric value between 0 and 1.", call. = FALSE)
  }

  # 5. Check lookup_na_state
  if (!is.logical(lookup_na_state) || length(lookup_na_state) != 1L || is.na(lookup_na_state)) {
    stop("'lookup_na_state' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # Convert to dataframe if necessary
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  # 6. Check long/lat columns if lookup_na_state is TRUE
  if (lookup_na_state) {
    if (!inherits(long, "character") || length(long) != 1L || !long %in% names(occ)) {
      stop("'long' must be provided and exist in 'occ' when 'lookup_na_state = TRUE'.", call. = FALSE)
    }
    if (!inherits(lat, "character") || length(lat) != 1L || !lat %in% names(occ)) {
      stop("'lat' must be provided and exist in 'occ' when 'lookup_na_state = TRUE'.", call. = FALSE)
    }
    if (all(is.na(occ[[long]]))) {
      occ[[long]] <- as.numeric(occ[[long]])
    }
    if (all(is.na(occ[[lat]]))) {
      occ[[lat]] <- as.numeric(occ[[lat]])
    }
    if (!is.numeric(occ[[long]]) || !is.numeric(occ[[lat]])) {
      stop("'long' and 'lat' columns must be numeric.", call. = FALSE)
    }
  }

  # 7. Check return_dictionary
  if (!is.logical(return_dictionary) || length(return_dictionary) != 1L || is.na(return_dictionary)) {
    stop("'return_dictionary' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # Get state dictionary
  ss <- getExportedValue("RuHere", "states_dictionary")

  # Remove accents
  occ[[state_column]] <- remove_accent(occ[[state_column]])

  # Convert empty cells, whitespace-only, or non-alphabetic symbols (like "?") to NA
  occ[[state_column]] <- trimws(occ[[state_column]])
  occ[[state_column]][!is.na(occ[[state_column]]) &
                        (occ[[state_column]] == "" | !grepl("[A-Za-z]", occ[[state_column]]))] <- NA

  # Set states to lower
  idx_lower <- nchar(occ[[state_column]]) > 3L & !is.na(occ[[state_column]])
  occ[[state_column]][idx_lower] <- tolower(occ[[state_column]][idx_lower])

  # Set codes to upper
  idx_upper <- nchar(occ[[state_column]]) <= 3L & !is.na(occ[[state_column]])
  occ[[state_column]][idx_upper] <- toupper(occ[[state_column]][idx_upper])

  # Check state names
  unique_states <- unique(occ[, c(state_column, country_column), drop = FALSE])
  valid_states <- as.character(na.omit(unique(unique_states[[state_column]])))
  ambiguous_matches <- NULL

  if (length(valid_states) == 0L) {
    final_states <- NULL
  } else {
    ccn <- florabr::match_names(species = valid_states,
                                species_to_match = ss$states_name$state_name,
                                max_distance = max_distance)
    colnames(ccn) <- c("state", "state_name", "Distance")

    # Join data and filter by country using dynamic country_column
    ccn <- merge(na.omit(ccn), ss$states_name, by = "state_name", all.x = TRUE)
    ccn <- ccn[ccn$country %in% unique_states[[country_column]], , drop = FALSE]
    ccn <- unique(ccn[, c("state", "state_suggested", "country"), drop = FALSE])

    if (nrow(ccn) > 0L) {
      colnames(ccn) <- c(state_column, "state_suggested", country_column)
    } else {
      ccn <- NULL
    }

    # Check state codes
    ccc <- ss$states_code[
      ss$states_code[["state_code"]] %in% valid_states &
        ss$states_code[["country"]] %in% unique_states[[country_column]],
      c("state_code", "state_suggested", "country"),
      drop = FALSE
    ]

    if (nrow(ccc) > 0L) {
      colnames(ccc) <- c(state_column, "state_suggested", country_column)
    } else {
      ccc <- NULL
    }

    # Combine and deduplicate
    all_matches <- unique(rbind(ccn, ccc))

    if (is.null(all_matches) || nrow(all_matches) == 0L) {
      final_states <- NULL
    } else {
      # Detect ambiguous matches (> 1 candidate suggested for the same (state, country) pair)
      pair_key <- paste(all_matches[[state_column]], all_matches[[country_column]], sep = "\r")
      dup_keys <- unique(pair_key[duplicated(pair_key)])

      if (length(dup_keys) > 0L) {
        is_amb <- pair_key %in% dup_keys
        ambiguous_matches <- all_matches[is_amb, , drop = FALSE]
        final_states <- all_matches[!is_amb, , drop = FALSE]
      } else {
        final_states <- all_matches
      }
    }
  }

  # Preserve original row order when merging unambiguous matches
  occ$.row_order <- seq_len(nrow(occ))
  if (is.null(final_states) || nrow(final_states) == 0L) {
    occ_final <- occ
    occ_final$state_suggested <- NA_character_
  } else {
    occ_final <- merge(occ, final_states, by = c(state_column, country_column),
                       all.x = TRUE, sort = FALSE)
    occ_final <- occ_final[order(occ_final$.row_order), , drop = FALSE]
  }
  occ_final$.row_order <- NULL
  rownames(occ_final) <- NULL

  # Relocate columns safely (protecting against column index 1)
  pos_state <- match(state_column, names(occ))
  if (!is.na(pos_state) && pos_state > 1L) {
    occ_final <- relocate_before(occ_final, state_column, names(occ)[pos_state - 1L])
  }
  occ_final <- relocate_after(occ_final, "state_suggested", state_column)

  pos_country <- match(country_column, names(occ))
  if (!is.na(pos_country) && pos_country > 1L) {
    occ_final <- relocate_before(occ_final, country_column, names(occ)[pos_country - 1L])
  }

  # Resolve ambiguous matches & fill NA from coordinates
  if (lookup_na_state) {
    # 1. Try to disambiguate multiple fuzzy candidates using coordinates
    if (!is.null(ambiguous_matches) && nrow(ambiguous_matches) > 0L) {
      amb_pairs <- unique(paste(ambiguous_matches[[state_column]],
                                ambiguous_matches[[country_column]], sep = "\r"))
      occ_pairs <- paste(occ_final[[state_column]],
                         occ_final[[country_column]], sep = "\r")

      amb_idx <- which(occ_pairs %in% amb_pairs &
                         !is.na(occ_final[[long]]) & !is.na(occ_final[[lat]]))

      if (length(amb_idx) > 0L) {
        occ_amb <- states_from_coords(occ_final[amb_idx, , drop = FALSE],
                                      long = long, lat = lat, from = "na_only",
                                      state_column = "state_suggested",
                                      output_column = "state_suggested",
                                      append_source = FALSE)

        # Keep coordinate-derived state ONLY if it matches one of the fuzzy candidates for that (state, country) pair
        for (k in seq_along(amb_idx)) {
          row_k <- amb_idx[k]
          in_st <- occ_final[[state_column]][row_k]
          in_ct <- occ_final[[country_column]][row_k]
          coord_state <- occ_amb$state_suggested[k]

          candidates <- ambiguous_matches$state_suggested[
            ambiguous_matches[[state_column]] == in_st &
              (is.na(ambiguous_matches[[country_column]]) & is.na(in_ct) |
                 !is.na(ambiguous_matches[[country_column]]) & !is.na(in_ct) &
                 ambiguous_matches[[country_column]] == in_ct)
          ]

          if (!is.na(coord_state) && coord_state %in% candidates) {
            occ_final$state_suggested[row_k] <- coord_state
          }
        }
      }
    }

    # 2. Fill any remaining NAs from coordinates
    occ_final <- states_from_coords(occ_final, long, lat, from = "na_only",
                                    state_column = "state_suggested",
                                    output_column = "state_suggested",
                                    append_source = TRUE)
    occ_final$state_source[
      is.na(occ_final$state_source) &
        !is.na(occ_final[["state_suggested"]])] <- "metadata"
  }

  # Return dictionary?
  if (return_dictionary) {
    resolved_pairs <- unique(occ_final[!is.na(occ_final[[state_column]]) &
                                         !is.na(occ_final[["state_suggested"]]),
                                       c(state_column, "state_suggested", country_column), drop = FALSE])

    all_non_na_inputs <- unique_states[!is.na(unique_states[[state_column]]),
                                       c(state_column, country_column), drop = FALSE]
    all_keys <- paste(all_non_na_inputs[[state_column]], all_non_na_inputs[[country_column]], sep = "\r")
    res_keys <- paste(resolved_pairs[[state_column]], resolved_pairs[[country_column]], sep = "\r")

    states_out <- all_non_na_inputs[!(all_keys %in% res_keys), , drop = FALSE]
    if (nrow(states_out) > 0L) {
      states_out$state_suggested <- NA_character_
      states_out <- states_out[, c(state_column, "state_suggested", country_column), drop = FALSE]
    } else {
      states_out <- NULL
    }

    r <- rbind(resolved_pairs, states_out)
    rownames(r) <- NULL
    return(list("occ" = occ_final,
                "report" = r))
  } else {
    return(occ_final)
  }
}
