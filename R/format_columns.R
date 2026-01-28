#' Format and standardize column names and data types of an occurrence dataset
#'
#' @param occ (data.frame or data.table) a dataset with occurrence records,
#' preferably obtained from `import_gbif()`, `get_specieslink()`, `get_bien()`,
#' or `get_idigbio()`.
#' @param metadata (character or data.frame) if a character, one of 'gbif',
#' 'specieslink', 'bien', or 'idigbio', specifying which metadata template to
#' use (the corresponding data frames are available in
#' `RuHere::prepared_metadata`). If a data.frame is provided, it must have 21
#' columns (see **Details**).
#' @param extract_binomial (logical) whether to create a column with the
#' binomial name of the species. If FALSE, it will create a column "species"
#' with the exact name stored in the scientificName column. Default is TRUE.
#' @param binomial_from (character) the column name in metadata from which to
#' extract the binomial name. Only applicable if `extract_binomial = TRUE`.
#' If `metadata` corresponds to one of the predefined sources ('gbif',
#' specieslink', 'bien', or 'idigbio'), predefined columns will be used
#' automatically. Default is "scientificName".
#' @param include_subspecies (logical) whether to include subspecies in the
#' binomial name.  Only applicable if `extract_binomial = TRUE`. If TRUE, the
#' function includes any infraspecific epithet after the pattern "subsp.".
#' Default if FALSE.
#' @param include_variety (logical) whether to include variety in the binomial
#' name. Only applicable if `extract_binomial = TRUE`. If TRUE, the function
#' includes any infraspecific epithet after the pattern "var.". Default if FALSE.
#' @param check_numeric (logical) whether to check and coerce the columns
#' specified in `numeric_columns` to numeric type. Default is TRUE.
#' @param numeric_columns (character) a vector of column names that must be
#' numeric. Default is NULL, meaning that if `check_numeric = TRUE`, the
#' following columns will be coerced: 'decimalLongitude', 'decimalLatitude',
#' 'coordinateUncertaintyInMeters', 'elevation', and 'year'.
#' @param check_encoding (logical) whether to check and fix the encoding of
#' columns that typically contain special characters (see **Details**). Default
#' is TRUE.
#' @param data_source (character) the source of the occurrence records. Default
#' is NULL, meaning it will use the same string provided in `metadata`. If
#' `metadata` is a user-defined data.frame, this argument must be specified.
#' @param progress_bar (logical) whether to display a progress bar during
#' processing. If TRUE, the 'pbapply' package must be installed. Default is
#' `FALSE`.
#' @param verbose (logical) whether to print messages about the progress.
#' Default is FALSE.
#'
#' @details
#' If a user-defined metadata data.frame is provided, it must include the
#' following 21 columns:
#' 'scientificName', 'collectionCode', 'catalogNumber', 'decimalLongitude',
#' 'decimalLatitude', 'coordinateUncertaintyInMeters', 'elevation', 'country',
#' 'stateProvince', 'municipality', 'locality', 'year', 'eventDate',
#' 'recordedBy', 'identifiedBy', 'basisOfRecord', 'occurrenceRemarks', 'habitat',
#' 'datasetName', 'datasetKey', and 'key'.
#'
#' If `check_encoding = TRUE`, the function will inspect and, if necessary, fix
#' the encoding of these columns:
#' 'collectionCode', 'catalogNumber', 'country', 'stateProvince',
#' municipality', 'locality', 'eventDate','recordedBy', 'identifiedBy',
#' 'basisOfRecord', and 'datasetName'.
#'
#' @returns
#' A data.frame with standardized column names and data types according to the
#' specified metadata.
#'
#' @importFrom florabr get_binomial
#' @importFrom stringi stri_enc_detect
#'
#' @export
#'
#' @examples
#' # Example with GBIF
#' data("occ_gbif", package = "RuHere") #Import data example
#' gbif_standardized <- format_columns(occ_gbif, metadata = "gbif")
#' # Example with SpeciesLink
#' data("occ_splink", package = "RuHere") #Import data example
#' splink_standardized <- format_columns(occ_splink, metadata = "specieslink")
#' # Example with BIEN
#' data("occ_bien", package = "RuHere") #Import data example
#' bien_standardized <- format_columns(occ_bien, metadata = "bien")
#' # Example with idigbio
#' data("occ_idig", package = "RuHere") #Import data example
#' idig_standardized <- format_columns(occ_idig, metadata = "idigbio")
#'
format_columns <- function(occ,
                           metadata,
                           extract_binomial = TRUE,
                           binomial_from = NULL,
                           include_subspecies = FALSE,
                           include_variety = FALSE,
                           check_numeric = TRUE,
                           numeric_columns = NULL,
                           check_encoding = TRUE,
                           data_source = NULL,
                           progress_bar = FALSE,
                           verbose = FALSE) {
  # ---- ARGUMENT CHECKING ----

  # 1. Check occ
  if (missing(occ) || !inherits(occ, c("data.frame", "data.table"))) {
    stop("'occ' must be a data.frame or data.table containing occurrence records.", call. = FALSE)
  }
  if (nrow(occ) == 0) {
    stop("'occ' is empty. Please provide a dataset with occurrence records.", call. = FALSE)
  }

  # 2. Check metadata
  if (missing(metadata) || is.null(metadata)) {
    stop("Argument 'metadata' is required. It must be either a string ('gbif', 'specieslink', 'bien', or 'idigbio') or a data.frame.", call. = FALSE)
  }

  if (inherits(metadata, "character")) {
    valid_sources <- c("gbif", "specieslink", "bien", "idigbio")
    if (!metadata %in% valid_sources) {
      stop("Invalid 'metadata' value. Must be one of: 'gbif', 'specieslink', 'bien', or 'idigbio'.", call. = FALSE)
    }
    meta_df <- getExportedValue("RuHere", "prepared_metadata")
    meta_df <- meta_df[[metadata]]

  } else if (inherits(metadata, "data.frame")) {
    # User-provided metadata
    required_cols <- c("scientificName", "collectionCode", "catalogNumber",
                       "decimalLongitude", "decimalLatitude",
                       "coordinateUncertaintyInMeters", "elevation", "country",
                       "stateProvince", "municipality", "locality", "year",
                       "eventDate", "recordedBy", "identifiedBy",
                       "basisOfRecord", "occurrenceRemarks", "habitat",
                       "datasetName", "datasetKey", "key")

    missing_cols <- setdiff(required_cols, names(metadata))
    if (length(missing_cols) > 0) {
      stop(paste0("The user-provided 'metadata' is missing the following required columns: ",
                  paste(missing_cols, collapse = ", ")), call. = FALSE)
    }
    meta_df <- metadata

  } else {
    stop("'metadata' must be either a character string or a data.frame.",
         call. = FALSE)
  }

  # 3. Check extract_binomial
  if (!inherits(extract_binomial, "logical") || length(extract_binomial) != 1) {
    stop("'extract_binomial' must be a single logical value (TRUE or FALSE).",
         call. = FALSE)
  }

  # 4. Check binomial_from
  if (extract_binomial) {
    if (!is.null(binomial_from) && !binomial_from %in% names(occ)) {
      stop(paste0("The column specified in 'binomial_from' ('", binomial_from,
                  "') was not found in 'occ'."), call. = FALSE)
    }
    if(!inherits(metadata, "character") && is.null(binomial_from)){
      stop("If 'extract_binomial' is TRUE and a user-defined metadata data.frame is provided, you must specify the column to extract the binomial species name", call. = FALSE)
    }

  }

  # 5. Check include_subspecies / include_variety
  if (!inherits(include_subspecies, "logical") || length(include_subspecies) != 1)
    stop("'include_subspecies' must be a single logical value.", call. = FALSE)
  if (!inherits(include_variety, "logical") || length(include_variety) != 1)
    stop("'include_variety' must be a single logical value.", call. = FALSE)

  # 6. Check check_numeric
  if (!inherits(check_numeric, "logical") || length(check_numeric) != 1)
    stop("'check_numeric' must be a single logical value.", call. = FALSE)

  # 7. Define numeric columns
  if (is.null(numeric_columns)) {
    numeric_columns <- c("decimalLongitude", "decimalLatitude",
                         "coordinateUncertaintyInMeters", "elevation", "year")
  } else {
    if (!inherits(numeric_columns, "character"))
      stop("'numeric_columns' must be a character vector of column names.", call. = FALSE)
  }

  # 8. Check check_encoding
  if (!inherits(check_encoding, "logical") || length(check_encoding) != 1)
    stop("'check_encoding' must be a single logical value.", call. = FALSE)

  # 9. Check data_source
  if (!is.null(data_source) && !inherits(data_source, "character"))
    stop("'data_source' must be a character string or NULL.", call. = FALSE)
  if (is.null(data_source)) {
    if (inherits(metadata, "character")) {
      data_source <- metadata
    } else {
      stop("When 'metadata' is a custom data.frame, 'data_source' must be provided.", call. = FALSE)
    }
  }

  # 10. Check verbose
  if (!inherits(verbose, "logical") || length(verbose) != 1)
    stop("'verbose' must be a single logical value (TRUE or FALSE).", call. = FALSE)

  # progress_bar
  if (!inherits(progress_bar, "logical") || length(progress_bar) != 1) {
    stop("'progress_bar' must be a single logical value (TRUE/FALSE).",
         call. = FALSE)
  }

  # Progress bar
  if (progress_bar) {
    if (requireNamespace("pbapply", quietly = TRUE)) {
      my_sapply <- pbapply::pbsapply
    } else {
      stop("Package 'pbapply' is required if 'progress_bar = TRUE'.
Run install.packages('pbapply')", call. = FALSE)
    }
  } else {
    my_sapply <- base::sapply
  }

  # Convert to dataframe if necessary
  if(inherits(occ, c("data.table", "tbl_df"))){
    occ <- as.data.frame(occ)}


  if(is.character(metadata)){
    prepared_d <- getExportedValue("RuHere", "prepared_metadata")
    d <- prepared_d[[metadata]]
  } else {
    d <- meta_df
  }

  if(is.null(numeric_columns)){
    numeric_columns <- intersect(c("decimalLongitude", "decimalLatitude",
            "coordinateUncertaintyInMeters", "elevation", "year"),
            colnames(d))
  }

  if(check_encoding){
    to_check <- intersect(c("collectionCode", "catalogNumber",
                 "country", "stateProvince", "municipality",
                 "locality", "eventDate", "recordedBy", "identifiedBy",
                 "basisOfRecord", "datasetName"),
                 colnames(d))
  }

  #Check columns
  c_met <- setdiff(na.omit(unlist(d[1,])), colnames(occ))

  # If column is not strictly necessary, create and fill with NA
  if(length(c_met) > 0){
    missing_info <- names(d[ sapply(d, function(x) any(x %in% c_met,
                                                       na.rm = TRUE))])

    if(!(missing_info %in% c("scientificName", "decimalLongitude",
                             "decimalLatitude"))){
      occ[, c_met] <- NA
      } else {
        stop("The following columns are missing in occ: ",
         paste(c_met, collapse = " | "))}
  }

  # Fix years
  if(inherits(metadata, "character")){
    if(metadata == "bien"){
      occ$year <- format(as.Date(occ$date_collected, format = "%Y-%m-%d"), "%Y")
      d[,"year"] <- "year"
    }

    if(metadata == "idigbio"){
      occ$year <- format(as.Date(occ$datecollected, format = "%Y-%m-%d"), "%Y")
      d[,"year"] <- "year"
    }
  }


  # Column to extract binomial species from kwonw metadata
  if(extract_binomial){
    if(inherits(metadata, "character")){
      if(is.null(binomial_from) && metadata %in% c("gbif", "specieslink", "idigbio",
                                                  "bien")){
        binomial_from <- if(metadata == "gbif"){
          "acceptedScientificName"
        } else if(metadata == "specieslink"){
          "scientificname"
        } else if(metadata == "bien"){
          "scrubbed_species_binomial"
        } else if(metadata == "idigbio"){
          "scientificname"
        }
      }

    }

    # Get binomial species
    if(!is.null(binomial_from)){
      # Check if columns exists
      if(!(binomial_from %in% colnames(occ))){
        stop("Columns specified in 'binomial_from' is not present in 'occ'")}

      #Get binomial
      species <- florabr::get_binomial(species_names = occ[[binomial_from]],
                                       include_subspecies = include_subspecies,
                                       include_variety = include_variety)
    }

  } else {
    species <- occ[, metadata$scientificName]
  }

  # Rename and remove columns...
  if(verbose)
    message("Renaming columns...")
  new_names <- as.list(d[1,])
   #Remove
  to_keep <- as.character(new_names)
  to_keep <- to_keep[to_keep != "NA"]
  occ <- occ[,intersect(colnames(occ), to_keep)]

  #Create columns, if necessary
  absent_columns <- setdiff(to_keep, colnames(occ))
  if(length(absent_columns)>0){
    occ[,absent_columns] <- NA}

  present_names <- new_names[!is.na(new_names)]
  absent_names <- names(new_names[is.na(new_names)])
  if(length(absent_names)>0){
    occ[,absent_names] <- NA}

  # Rename columns
  old_names <- unlist(present_names, use.names = FALSE)
  new_names <- names(present_names)
  names(occ)[ match(old_names, names(occ)) ] <- new_names

  #Create columns with data_source
  if(is.null(data_source) & inherits(metadata, "character")){
    data_source <- metadata
  }

  occ$data_source <- data_source

  if(extract_binomial){
    occ$species <- species
  }


  if(check_numeric){

  if(verbose)
    message("Fixing numeric columns...")

  #Character columns
  c_c <- setdiff(colnames(occ), numeric_columns)
  occ[numeric_columns] <- lapply(occ[numeric_columns], function(x) as.numeric(x))
  occ[c_c] <- lapply(occ[c_c], function(x) as.character(x))
  }

  #Fix encoding
  if(check_encoding){
    if(verbose)
      message("Fixing encoding of character columns...")

  for (column in to_check){
    if(verbose)
      message("Checking encoding of ", column)
    #Check if is valid
    cc_is_valid <- validUTF8(occ[[column]])

    #Get values that are not valid
    to_fix_cc <- unique(occ[[column]][!cc_is_valid])

    if(length(to_fix_cc) > 0) {
      #Get correct encoding
      cc_correct <- stringi::stri_enc_detect(to_fix_cc)
      names(cc_correct) <- to_fix_cc
      #Fix
      if(verbose)
        message("Fixing encoding of ", column)
      cc_corrected <- my_sapply(names(cc_correct), function(i){
        try({cc_corrected_i <- iconv(i,
                                     from = cc_correct[[i]]$Encoding[1],
                                     to = "utf-8")
        df_cc <- data.frame(old = i,
                            new = cc_corrected_i)
        return(df_cc)})
      })
    }

  #Get index to fix collection code
  ind_to_fix_cc <- which(!cc_is_valid)

  unique(occ[[column]][ind_to_fix_cc])

  } #End of check encoding
  }
  #Append data_source
  occ$data_source = data_source

  # Create ID for each records
  occ$record_id <- paste(occ$data_source, 1:nrow(occ), sep = "_")

  # Fix encoding again
  bad_cols <- names(occ)[
    sapply(occ, function(x) {
      if (is.character(x) || is.factor(x)) {
        tryCatch({
          nchar(as.character(x))
          FALSE
        }, error = function(e) TRUE)
      } else {
        FALSE
      }
    })
  ]
  if(length(bad_cols) > 0){
    for(i in bad_cols){
      occ[[i]] <- iconv(occ[[i]], from = "", to = "UTF-8")
    }
  }


  if(!extract_binomial){
    return(occ[,c("record_id", colnames(d), "data_source")])
  } else {
    return(occ[,c("record_id", "species", colnames(d), "data_source")])
  }
}

