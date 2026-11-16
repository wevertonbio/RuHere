#' Standardize country names
#'
#' @description
#' This function standardizes country names using both names and codes present
#' in a specified column.
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records,
#' preferably standardized using `format_columns()`.
#' @param country_column (character) the column name containing the country
#' information. Default is `"country"`.
#' @param max_distance (numeric) maximum allowed distance (as a fraction) when
#' searching for suggestions for misspelled country names. Can be any value
#' between 0 and 1. Higher values return more suggestions. See `agrep()` for
#' details. Default is `0.1`.
#' @param user_dictionary (data.frame) optional data.frame with two columns:
#' `'country_name'` and `'country_suggested'`. If provided, this dictionary will
#' be combined with the package's default country dictionary
#' (`RuHere::country_dictionary`). Default is `NULL`.
#' @param lookup_na_country (logical) whether to extract the country from
#' coordinates when the country column has missing values or ambiguous fuzzy
#' matches. If `TRUE`, longitude and latitude columns must be provided. Default
#' is `FALSE`.
#' @param long (character) column name with longitude. Only applicable if
#' `lookup_na_country = TRUE`. Default is `"decimalLongitude"`.
#' @param lat (character) column name with latitude. Only applicable if
#' `lookup_na_country = TRUE`. Default is `"decimalLatitude"`.
#' @param return_dictionary (logical) whether to return the dictionary of
#' countries that were (fuzzy) matched. Default is `TRUE`.
#'
#' @details
#' Country names are first standardized by exact matching against a list of
#' country names in several languages from `rnaturalearthdata::map_units110`.
#' Any unmatched names are then processed using a fuzzy matching algorithm to
#' find potential candidates for misspelled country names. When fuzzy
#' matching yields multiple candidate countries for a single input string, the
#' ambiguity is resolved using geographic coordinates if `lookup_na_country = TRUE`
#' (retaining the candidate that matches the point's location), or set to `NA`
#' when coordinates are unavailable or `lookup_na_country = FALSE`. If unmatched
#' names remain and `lookup_na_country = TRUE`, the country is extracted from
#' coordinates using a map retrieved from `rnaturalearthdata::map_units110`.
#'
#' @returns
#' If `return_dictionary = FALSE`, returns the original `occ` `data.frame`
#' augmented with a `country_suggested` column (and `country_source` if
#' `lookup_na_country = TRUE`).
#' If `return_dictionary = TRUE`, returns a list with two elements:
#' \item{occ}{The original `occ` data.frame with the `country_suggested` column.}
#' \item{report}{A data.frame with the original country names and the suggested
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
standardize_countries <- function(occ,
                                  country_column = "country",
                                  max_distance = 0.1,
                                  user_dictionary = NULL,
                                  lookup_na_country = FALSE,
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

  # 2. Check country_column
  if (!inherits(country_column, "character") || length(country_column) != 1L || is.na(country_column)) {
    stop("'country_column' must be a single character string specifying the column with country information.", call. = FALSE)
  }
  if (!country_column %in% names(occ)) {
    stop(paste0("The column specified in 'country_column' ('", country_column, "') was not found in 'occ'."), call. = FALSE)
  }

  # 3. Check max_distance
  if (!is.numeric(max_distance) || length(max_distance) != 1L || is.na(max_distance) || max_distance < 0 || max_distance > 1) {
    stop("'max_distance' must be a numeric value between 0 and 1.", call. = FALSE)
  }

  # 4. Check user_dictionary
  if (!is.null(user_dictionary)) {
    if (!inherits(user_dictionary, "data.frame")) {
      stop("'user_dictionary' must be a data.frame with columns 'country_name' and 'country_suggested'.", call. = FALSE)
    }
    required_dict_cols <- c("country_name", "country_suggested")
    missing_dict_cols <- setdiff(required_dict_cols, names(user_dictionary))
    if (length(missing_dict_cols) > 0L) {
      stop(paste0("'user_dictionary' is missing required columns: ", paste(missing_dict_cols, collapse = ", ")), call. = FALSE)
    }
  }

  # 5. Check lookup_na_country
  if (!is.logical(lookup_na_country) || length(lookup_na_country) != 1L || is.na(lookup_na_country)) {
    stop("'lookup_na_country' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # Convert to dataframe if necessary
  if (inherits(occ, "data.table")) {
    occ <- as.data.frame(occ)
  }

  # 6. Check long/lat columns if lookup_na_country is TRUE
  if (lookup_na_country) {
    if (!inherits(long, "character") || length(long) != 1L || !long %in% names(occ)) {
      stop("'long' must be provided and exist in 'occ' when 'lookup_na_country = TRUE'.", call. = FALSE)
    }
    if (!inherits(lat, "character") || length(lat) != 1L || !lat %in% names(occ)) {
      stop("'lat' must be provided and exist in 'occ' when 'lookup_na_country = TRUE'.", call. = FALSE)
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

  # Get country dictionary
  cd <- getExportedValue("RuHere", "country_dictionary")

  # Bind user dictionary
  if (!is.null(user_dictionary)) {
    cd$country_name <- rbind(cd$country_name, user_dictionary)
  }

  # Remove accents
  occ[[country_column]] <- remove_accent(occ[[country_column]])

  # Replace empty values, whitespace-only, or non-alphabetic symbols (like "?") with NA
  occ[[country_column]] <- trimws(occ[[country_column]])
  occ[[country_column]][!is.na(occ[[country_column]]) &
                          (occ[[country_column]] == "" | !grepl("[A-Za-z]", occ[[country_column]]))] <- NA

  # Set countries to lower
  idx_lower <- nchar(occ[[country_column]]) > 3L & !is.na(occ[[country_column]])
  occ[[country_column]][idx_lower] <- tolower(occ[[country_column]][idx_lower])

  # Set codes to upper
  idx_upper <- nchar(occ[[country_column]]) <= 3L & !is.na(occ[[country_column]])
  occ[[country_column]][idx_upper] <- toupper(occ[[country_column]][idx_upper])

  # Check country names
  unique_countries <- as.character(na.omit(unique(occ[[country_column]])))
  ambiguous_matches <- NULL

  if (length(unique_countries) == 0L) {
    final_countries <- data.frame(
      country = character(0),
      country_suggested = character(0),
      stringsAsFactors = FALSE
    )
    colnames(final_countries)[1L] <- country_column
  } else {
    ccn <- florabr::match_names(species = unique_countries,
                                species_to_match = cd$country_name$country_name,
                                max_distance = max_distance)

    colnames(ccn) <- c("country", "country_name", "Distance")
    ccn <- unique(
      merge(na.omit(ccn), cd$country_name, by = "country_name", all.x = TRUE)[
        , c("country", "country_suggested"), drop = FALSE]
    )

    if (nrow(ccn) > 0L) {
      colnames(ccn) <- c(country_column, "country_suggested")
    } else {
      ccn <- NULL
    }

    # Check country codes
    ccc <- cd$country_code
    ccc <- ccc[ccc$country_code %in% unique_countries, , drop = FALSE]

    if (nrow(ccc) > 0L) {
      colnames(ccc) <- c(country_column, "country_suggested")
    } else {
      ccc <- NULL
    }

    # Combine and deduplicate
    all_matches <- unique(rbind(ccn, ccc))

    if (is.null(all_matches) || nrow(all_matches) == 0L) {
      final_countries <- data.frame(
        country = character(0),
        country_suggested = character(0),
        stringsAsFactors = FALSE
      )
      colnames(final_countries)[1L] <- country_column
    } else {
      # Detect ambiguous matches (> 1 candidate suggested for the same input string)
      dup_inputs <- unique(all_matches[[country_column]][duplicated(all_matches[[country_column]])])

      if (length(dup_inputs) > 0L) {
        ambiguous_matches <- all_matches[all_matches[[country_column]] %in% dup_inputs, , drop = FALSE]
        final_countries <- all_matches[!(all_matches[[country_column]] %in% dup_inputs), , drop = FALSE]
      } else {
        final_countries <- all_matches
      }
    }
  }

  # Preserve original row order when merging unambiguous matches
  occ$.row_order <- seq_len(nrow(occ))
  if (nrow(final_countries) == 0L) {
    occ_final <- occ
    occ_final$country_suggested <- NA_character_
  } else {
    occ_final <- merge(occ, final_countries, by = country_column, all.x = TRUE, sort = FALSE)
    occ_final <- occ_final[order(occ_final$.row_order), , drop = FALSE]
  }
  occ_final$.row_order <- NULL
  rownames(occ_final) <- NULL

  # Relocate columns (if necessary)
  pos_country <- match(country_column, names(occ))
  if (pos_country > 1L) {
    occ_final <- relocate_before(
      occ_final, country_column, names(occ)[pos_country - 1L]
    )
  }
  occ_final <- relocate_after(occ_final, "country_suggested", country_column)

  # Resolve ambiguous matches & fill NA from coordinates
  if (lookup_na_country) {
    # 1. Try to disambiguate multiple fuzzy candidates using coordinates
    if (!is.null(ambiguous_matches) && nrow(ambiguous_matches) > 0L) {
      amb_idx <- which(occ_final[[country_column]] %in% unique(ambiguous_matches[[country_column]]) &
                         !is.na(occ_final[[long]]) & !is.na(occ_final[[lat]]))

      if (length(amb_idx) > 0L) {
        occ_amb <- country_from_coords(occ_final[amb_idx, , drop = FALSE],
                                       long = long, lat = lat,
                                       country_column = "country_suggested",
                                       output_column = "country_suggested",
                                       from = "na_only", append_source = FALSE)

        # Keep coordinate-derived country ONLY if it matches one of the fuzzy candidates
        for (k in seq_along(amb_idx)) {
          row_k <- amb_idx[k]
          in_str <- occ_final[[country_column]][row_k]
          coord_country <- occ_amb$country_suggested[k]
          candidates <- ambiguous_matches$country_suggested[ambiguous_matches[[country_column]] == in_str]

          if (!is.na(coord_country) && coord_country %in% candidates) {
            occ_final$country_suggested[row_k] <- coord_country
          }
        }
      }
    }

    # 2. Fill any remaining NAs from coordinates
    occ_final <- country_from_coords(occ_final, long, lat,
                                     country_column = "country_suggested",
                                     output_column = "country_suggested",
                                     from = "na_only", append_source = TRUE)

    occ_final$country_source[
      is.na(occ_final$country_source) &
        !is.na(occ_final[["country_suggested"]])] <- "metadata"
  }

  # Return dictionary?
  if (return_dictionary) {
    resolved_pairs <- unique(occ_final[!is.na(occ_final[[country_column]]) &
                                         !is.na(occ_final[["country_suggested"]]),
                                       c(country_column, "country_suggested"), drop = FALSE])
    countries_out <- setdiff(unique_countries, resolved_pairs[[country_column]])
    if (length(countries_out) > 0L) {
      countries_out_df <- data.frame(
        country = countries_out,
        country_suggested = NA_character_,
        stringsAsFactors = FALSE
      )
      colnames(countries_out_df)[1L] <- country_column
    } else {
      countries_out_df <- NULL
    }
    r <- rbind(resolved_pairs, countries_out_df)
    rownames(r) <- NULL
    return(list("occ" = occ_final,
                "report" = r))
  } else {
    return(occ_final)
  }
}
