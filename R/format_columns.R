format_columns <- function(occ,
                           metadata,
                           check_numeric = TRUE,
                           numeric_columns = NULL,
                           check_encoding = NULL,
                           extract_year_from = NULL,
                           data_source = NULL,
                           verbose = TRUE) {
  # Convert to dataframe
  if(!inherits(occ, "data.frame")){
    occ <- as.data.frame(occ)}


  if(is.character(metadata)){
    prepared_d <- RuHere::prepared_metadata
    d <- prepared_metadata[[metadata]]
  }

  if(is.null(numeric_columns)){
    numeric_columns <- intersect(c("decimalLongitude", "decimalLatitude",
            "coordinateUncertaintyInMeters", "elevation", "year"),
            colnames(d))
  }

  if(is.null(check_encoding)){
    check_encoding <- intersect(c("collectionCode", "catalogNumber",
                 "country", "stateProvince", "municipality",
                 "locality", "eventDate", "recordedBy", "identifiedBy",
                 "basisOfRecord", "datasetName"),
                 colnames(d))
  }

  #Check occ
  c_met <- setdiff(na.omit(unlist(d[1,])), colnames(occ))
  if(length(c_met) > 0){
    stop("The following columns are missing in occ: ",
         paste(c_met, collapse = " | "))
  }

  # Fix years
  if(metadata == "bien"){
    occ$year <- as.Date(occ$date_collected, format = "%Y-%m-%d") %>%
      format(., "%Y")
    d[,"year"] <- "year"
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
  occ <- occ %>% dplyr::rename(!!!present_names) %>% as.data.frame()

  #Create columns with data_source
  if(is.null(data_source)){
    data_source <- metadata
  }

  occ$data_source <- data_source

  if(check_numeric){

  if(verbose)
    message("Fixing numeric columns...")

  #Character columns
  c_c <- setdiff(colnames(occ), numeric_columns)
  occ[numeric_columns] <- lapply(occ[numeric_columns], function(x) as.numeric(x))
  occ[c_c] <- lapply(occ[c_c], function(x) as.character(x))
  }

  #Fix encoding
  if(!is.null(check_encoding)){
    if(verbose)
      message("Fixing encoding of character columns...")

  for (column in check_encoding){
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
      cc_corrected <- pbapply::pbsapply(names(cc_correct), function(i){
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

  occ[[column]][ind_to_fix_cc] %>% unique()

  } #End of check encoding
  }
  #Append data_source
  occ$data_source = data_source

  # Create ID for each records
  occ$record_id <- paste(occ$data_source, 1:nrow(occ), sep = "_")

  return(occ[,c("record_id", colnames(d), "data_source")])
}

# For test
# SpeciesLink
# occ <- RuHere::occ_splink
# occ_splink <- format_columns(occ = occ, metadata = "specieslink")
# # # GBIF
# occ <- RuHere::occ_gbif
# occ_gbif <- format_columns(occ = occ, metadata = "gbif")
# # BIEN
# occ <- RuHere::occ_bien
# occ_bien <- format_columns(occ = occ, metadata = "bien")
#
#
# all_occ <- bind_rows(occ_splink, occ_gbif, occ_bien)

# # Test function
# occ <- RuHere::occ_bien
# metadata <- "bien"
# numeric_columns = NULL
# check_encoding = NULL
# extract_year_from = NULL
# data_source = NULL
# verbose = TRUE

