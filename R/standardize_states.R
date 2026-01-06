#' Standardize state names
#'
#' @description
#' This function standardizes state names using both names and codes present
#' in a specified column.
#'
#' @param occ (data.frame) a dataset with occurrence records, preferably
#' standardized using `format_columns()`.
#' @param state_column (character) the column name containing the state
#' information.
#' @param country_column (character) the column name containing the country
#' information.
#' @param max_distance (numeric) maximum allowed distance (as a fraction) when
#' searching for suggestions for misspelled state names. Can be any value
#' between 0 and 1. Higher values return more suggestions. See `agrep()` for
#' details. Default is 0.1.
#' @param lookup_na_state (logical) whether to extract the state from
#' coordinates when the state column has missing values. If TRUE, longitude
#' and latitude columns must be provided. Default is FALSE.
#' @param long (character) column name with longitude. Only applicable if
#' `lookup_na_state = TRUE`. Default is "decimalLongitude".
#' @param lat (character) column name with latitude. Only applicable if
#' `lookup_na_state = TRUE`. Default is "decimalLatitude".
#' @param return_dictionary (logical) whether to return the dictionary of
#' states that were (fuzzy) matched.
#'
#' @details
#' States names are first standardized by exact matching against a list of
#' state names in several languages from `rnaturalearthdata::states50`.
#' Any unmatched names are then processed using a fuzzy matching algorithm to
#' find potential candidates for misspelled state names. If unmatched names
#' remain and `lookup_na_state = TRUE`, the state is extracted from
#' coordinates using a map retrieved from `rnaturalearthdata::states50`.
#'
#' @returns
#' A list with two elements:
#' \item{data}{The original `occ` data.frame with an additional column
#' (state_suggested) containing the suggested state names based on exact
#' match, fuzzy match, and/or coordinates.}
#' \item{dictionary}{If `return_dictionary = TRUE`, a data.frame with the
#' original state names and the suggested matches.}
#'
#' @importFrom florabr match_names
#' @export
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
                               long = "decimalLongitude", lat = "decimalLatitude",
                               return_dictionary = TRUE){
  # ---- ARGUMENT CHECKING ----

  # 1. Check occ
  if (missing(occ) || !inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame containing occurrence records.", call. = FALSE)
  }
  if (nrow(occ) == 0) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.", call. = FALSE)
  }

  # 2. Check state_column
  if (!inherits(state_column, "character") || length(state_column) != 1) {
    stop("'state_column' must be a single character string specifying the column with state information.", call. = FALSE)
  }
  if (!state_column %in% names(occ)) {
    stop(paste0("The column specified in 'state_column' ('", state_column, "') was not found in 'occ'."), call. = FALSE)
  }

  # 3. Check country_column
  if (!inherits(country_column, "character") || length(country_column) != 1) {
    stop("'country_column' must be a single character string specifying the column with country information.", call. = FALSE)
  }
  if (!country_column %in% names(occ)) {
    stop(paste0("The column specified in 'country_column' ('", country_column, "') was not found in 'occ'."), call. = FALSE)
  }

  # 4. Check max_distance
  if (!inherits(max_distance, "numeric") || length(max_distance) != 1 || max_distance < 0 || max_distance > 1) {
    stop("'max_distance' must be a numeric value between 0 and 1.", call. = FALSE)
  }

  # 5. Check lookup_na_state
  if (!inherits(lookup_na_state, "logical") || length(lookup_na_state) != 1) {
    stop("'lookup_na_state' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # 6. Check long/lat columns if lookup_na_state is TRUE
  if (lookup_na_state) {
    if (!inherits(long, "character") || !long %in% names(occ)) {
      stop("'long' must be provided and exist in 'occ' when 'lookup_na_state = TRUE'.", call. = FALSE)
    }
    if (!inherits(lat, "character") || !lat %in% names(occ)) {
      stop("'lat' must be provided and exist in 'occ' when 'lookup_na_state = TRUE'.", call. = FALSE)
    }
    if (!inherits(occ[[long]], "numeric") || !inherits(occ[[lat]], "numeric")) {
      stop("'long' and 'lat' columns must be numeric.", call. = FALSE)
    }
  }

  # 7. Check return_dictionary
  if (!inherits(return_dictionary, "logical") || length(return_dictionary) != 1) {
    stop("'return_dictionary' must be a single logical value (TRUE or FALSE).", call. = FALSE)
  }

  # Convert to dataframe if necessary
  if(inherits(occ, "data.table"))
    occ <- as.data.frame(occ)


  # Get state dictionary
  ss <- getExportedValue("RuHere", "states_dictionary")

  # Remove accents
  occ[[state_column]] <- remove_accent(occ[[state_column]])

  # Convert empty cells ("") in NA
  occ[[state_column]][occ[[state_column]] == ""] <- NA

  # Set states to lower
  occ[[state_column]][nchar(occ[[state_column]]) > 3 &
                          !is.na(occ[[state_column]])] <- tolower(
                            occ[[state_column]][nchar(occ[[state_column]]) > 3 &
                                                    !is.na(occ[[state_column]])])

  # Set codes to upper
  occ[[state_column]][nchar(occ[[state_column]]) <= 3 &
                          !is.na(occ[[state_column]])] <- toupper(
                            occ[[state_column]][nchar(occ[[state_column]]) <= 3 &
                                                    !is.na(occ[[state_column]])])

  # Replace empty values with NA
  occ[[state_column]][occ[[state_column]] == ""] <- NA

  # Check state names
  unique_states <- unique(occ[, c(state_column, country_column)])

  if(all(is.na(unique_states$stateProvince))){
    ccn <- NULL
    ccc <- NULL
    final_states <- NULL
  } else {
    ccn <- florabr::match_names(species = na.omit(unique_states[[state_column]]),
                                species_to_match = ss$states_name$state_name,
                                max_distance = max_distance)
    colnames(ccn) <- c("state", "state_name", "Distance")
    # Join data
    ccn <-merge(na.omit(ccn), ss$states_name, by = "state_name", all.x = TRUE)
    # Remove duplicated countries
    ccn <- ccn[ccn$country %in% unique_states$country_suggested,]
    ccn <- unique(ccn[, c("state", "state_suggested", "country")])


    if(nrow(ccn) > 0){
      # Rename columns
      colnames(ccn) <- c(state_column, "state_suggested", country_column)} else {
        ccn <- NULL
      }

    # Check state codes
    colnames(ss$states_code) <- c("state_code", state_column, country_column)

    ccc <- ss$states_code
    ccc <- ccc[ccc$state_code %in% unique_states$stateProvince, ]

    ccc <- ss$states_code[
      ss$states_code[["state_code"]] %in% unique_states[[state_column]] &
        ss$states_code[[country_column]] %in% unique_states[[country_column]],
    ]


    if(nrow(ccc) > 0){
      # Rename columns
      colnames(ccc) <- c(state_column, "state_suggested", country_column)} else {
        ccc <- NULL
      }

    # Join information
    final_states <- rbind(ccn, ccc)
  }


  if(is.null(final_states) || nrow(final_states) == 0){
    occ_final <- occ
    occ_final$state_suggested <- NA } else {
    occ_final <- merge(occ, final_states, by = c(state_column, country_column),
                       all.x = TRUE)
  }

  # Relocate columns
  occ_final <- relocate_before(occ_final, state_column,
                               names(occ)[which(names(occ) == state_column) - 1]
                               )
  occ_final <- relocate_after(occ_final, "state_suggested", state_column)

  # Fill NA?
  if(lookup_na_state){
    occ_final <- states_from_coords(occ_final, long, lat, from = "na_only",
                                     state_column = "state_suggested",
                                     output_column = "state_suggested",
                                     append_source = TRUE)
    occ_final$state_source[
      is.na(occ_final$state_source) &
        !is.na(occ_final[["state_suggested"]])] <- "metadata"
  }

  if(return_dictionary){
    states_out <- setdiff(unique_states[[state_column]],
                          final_states[[state_column]])
    states_out <- unique_states[unique_states[[state_column]] %in% states_out, ]
    states_out <- states_out[!is.na(states_out[[state_column]]), ]

    if(nrow(states_out) > 0) {
      states_out$state_suggested <- NA } else {
        states_out <- NULL
        }
    r <- rbind(final_states, states_out)
    return(list("occ" = occ_final,
                "report" = r))
  } else {#End of return_dictionary
    return(occ_final)
  }
} #End of function

