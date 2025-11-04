#' Standardize country names
#'
#' @description
#' This function standardizes country names using both names and codes present
#' in a specified column.
#'
#' @param occ (data.frame) a dataset with occurrence records,
#' preferably standardized using `format_columns()`.
#' @param country_column (character) the column name containing the country
#' information.
#' @param max_distance (numeric) maximum allowed distance (as a fraction) when
#' searching for suggestions for misspelled country names. Can be any value
#' between 0 and 1. Higher values return more suggestions. See `agrep()` for
#' details. Default is 0.1.
#' @param user_dictionary (data.frame) optional data.frame with two columns:
#' 'country_name' and 'country_suggested'. If provided, this dictionary will be
#' combined with the package’s default country dictionary
#' (`RuHere::country_dictionary`). Default is NULL.
#' @param lookup_na_country (logical) whether to extract the country from
#' coordinates when the country column has missing values. If TRUE, longitude
#' and latitude columns must be provided. Default is FALSE.
#' @param long (character) column name with longitude. Only applicable if
#' `lookup_na_country = TRUE`. Default is NULL.
#' @param lat (character) column name with latitude. Only applicable if
#' `lookup_na_country = TRUE`. Default is NULL.
#' @param return_dictionary (logical) whether to return the dictionary of
#' countries that were (fuzzy) matched.
#'
#' @details
#' Country names are first standardized by exact matching against a list of
#' country names in several languages from `rnaturalearthdata::map_units110`.
#' Any unmatched names are then processed using a fuzzy matching algorithm to
#' find potential candidates for misspelled country names. If unmatched names
#' remain and `lookup_na_country = TRUE`, the country is extracted from
#' coordinates using a map retrieved from `rnaturalearthdata::map_units110`.
#'
#'
#' @returns
#' A list with two elements:
#' \item{data}{The original `occ` data.frame with an additional column
#' (country_suggested) containing the suggested country names based on exact
#' match, fuzzy match, and/or coordinates.}
#' \item{dictionary}{If `return_dictionary = TRUE`, a data.frame with the
#' original country names and the suggested matches.}
#'
#' @importFrom florabr match_names
#' @importFrom dplyr left_join %>% select distinct filter bind_rows left_join
#' all_of relocate
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
standardize_countries <- function(occ,
                                  country_column = "country",
                                  max_distance = 0.1,
                                  user_dictionary = NULL,
                                  lookup_na_country = FALSE,
                                  long = NULL, lat = NULL,
                                  return_dictionary = TRUE){

  # ---- ARGUMENT CHECKING ----

  # 1. Check occ
  if (missing(occ) || !inherits(occ, "data.frame")) {
    stop("'occ' must be a data.frame containing occurrence records.", call. = FALSE)
  }
  if (nrow(occ) == 0) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.", call. = FALSE)
  }

  # 2. Check country_column
  if (missing(country_column) || !is.character(country_column) || length(country_column) != 1) {
    stop("'country_column' must be a single character string specifying the column with country information.", call. = FALSE)
  }
  if (!country_column %in% names(occ)) {
    stop(paste0("The column specified in 'country_column' ('", country_column, "') was not found in 'occ'."), call. = FALSE)
  }

  # 3. Check max_distance
  if (!is.numeric(max_distance) || length(max_distance) != 1 || max_distance < 0 || max_distance > 1) {
    stop("'max_distance' must be a numeric value between 0 and 1.", call. = FALSE)
  }

  # 4. Check user_dictionary
  if (!is.null(user_dictionary)) {
    if (!is.data.frame(user_dictionary)) {
      stop("'user_dictionary' must be a data.frame with columns 'country_name' and 'country_suggested'.", call. = FALSE)
    }
    required_dict_cols <- c("country_name", "country_suggested")
    missing_dict_cols <- setdiff(required_dict_cols, names(user_dictionary))
    if (length(missing_dict_cols) > 0) {
      stop(paste0("'user_dictionary' is missing required columns: ", paste(missing_dict_cols, collapse = ", ")), call. = FALSE)
    }
  }

  # 5. Check lookup_na_country
  if (!is.logical(lookup_na_country) || length(lookup_na_country) != 1) {
    stop("'lookup_na_country' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # 6. Check long/lat columns if lookup_na_country is TRUE
  if (lookup_na_country) {
    if (is.null(long) || !long %in% names(occ)) {
      stop("'long' must be provided and exist in 'occ' when 'lookup_na_country = TRUE'.", call. = FALSE)
    }
    if (is.null(lat) || !lat %in% names(occ)) {
      stop("'lat' must be provided and exist in 'occ' when 'lookup_na_country = TRUE'.", call. = FALSE)
    }
    if (!is.numeric(occ[[long]]) || !is.numeric(occ[[lat]])) {
      stop("'long' and 'lat' columns must be numeric.", call. = FALSE)
    }
  }

  # 7. Check return_dictionary
  if (!is.logical(return_dictionary) || length(return_dictionary) != 1) {
    stop("'return_dictionary' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }


  # Get country dictionary
  cd <- getExportedValue("RuHere", "country_dictionary")

  # Bind user dictionary
  if(!is.null(user_dictionary)){
    cd$country_name <- rbind(cd$country_name, user_dictionary)
  }


  # Remove accents
  occ[[country_column]] <- remove_accent(occ[[country_column]])


  # Set countries to lower
  occ[[country_column]][nchar(occ[[country_column]]) > 3 &
                          !is.na(occ[[country_column]])] <- tolower(
    occ[[country_column]][nchar(occ[[country_column]]) > 3 &
                            !is.na(occ[[country_column]])])

  # Set codes to upper
  occ[[country_column]][nchar(occ[[country_column]]) <= 3 &
                          !is.na(occ[[country_column]])] <- toupper(
                            occ[[country_column]][nchar(occ[[country_column]]) <= 3 &
                                                    !is.na(occ[[country_column]])])

  # Check country names
  unique_countries <- na.omit(unique(occ[[country_column]]))

  ccn <- florabr::match_names(species = unique_countries,
                               species_to_match = cd$country_name$country_name,
                               max_distance = max_distance)

  colnames(ccn) <- c("country", "country_name", "Distance")
  # Join data
  ccn <- dplyr::left_join(na.omit(ccn), cd$country_name, by = "country_name") %>%
    dplyr::select(country, country_suggested) %>% dplyr::distinct()

  if(nrow(ccn) > 0){
    # Rename columns
    colnames(ccn) <- c(country_column, "country_suggested")} else {
      ccn <- NULL
    }

  # Check country codes
  ccc <- cd$country_code %>% dplyr::filter(country_code %in% unique_countries)

  if(nrow(ccc) > 0){
    # Rename columns
    colnames(ccc) <- c(country_column, "country_suggested")} else {
      ccc <- NULL
    }

  # Join information
  final_countries <- dplyr::bind_rows(ccn, ccc)

  if(nrow(final_countries) > 0){
  occ_final <- dplyr::left_join(occ, final_countries, by = country_column)
  } else {
      occ_final <- occ
      occ_final$country_suggested <- NA
  }

  # Relocate columns
  occ_final <- occ_final %>%
    dplyr::relocate(country_suggested, .after = dplyr::all_of(country_column))

  # Fill NA?
  if(lookup_na_country){
    occ_final <- country_from_coords(occ_final, long, lat,
                                      country_column = "country_suggested",
                                      output_column = "country_suggested",
                                      from = "na_only", append_source = TRUE)
    occ_final$country_source[
      is.na(occ_final$country_source) &
        !is.na(occ_final[["country_suggested"]])] <- "metadata"
  }


  # Return dictionary?
  if(return_dictionary){
    countries_out <- setdiff(unique_countries, final_countries$country)
    if(length(countries_out) > 0) {
      countries_out <- data.frame("country" = countries_out,
                                  "country_suggested" = NA) } else {
        countries_out <- NULL
      }
      r <- dplyr::bind_rows(final_countries,
                     countries_out)
      return(list("occ" = occ_final,
                  "report" = r))
  } else {#End of return_dictionary
    return(occ_final)
  }
} #End of function
