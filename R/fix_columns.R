#Fix columns
fix_columns <- function(data, metadata,
                        numeric_columns = NULL,
                        check_encoding = NULL,
                        data_source, verbose = TRUE) {
  data <- as.data.frame(data)

  #Check data
  c_met <- setdiff(na.omit(t(metadata)), colnames(data))
  if(length(c_met) > 0){
    stop("The following columns are missing in data: ",
         paste(c_met, collapse = " | "))
  }

  if(verbose)
    message("Renaming columns...")
  new_names <- as.list(metadata[1,])

  #Remove columns
  a <- as.character(new_names)
  a <- a[a != "NA"]
  data <- data[,intersect(colnames(data), a)]

  #Create columns, if necessary
  absent_columns <- setdiff(a, colnames(data))
  if(length(absent_columns)>0){
    data[,absent_columns] <- NA}

  present_names <- new_names[!is.na(new_names)]
  absent_names <- names(new_names[is.na(new_names)])
  if(length(absent_names)>0){
    data[,absent_names] <- NA}

  # Rename columns
  data <- data %>% dplyr::rename(!!!present_names) %>% as.data.frame()
  # # Create columns, if necessary
  # if(length(absent_names) > 0) {
  # data[,absent_names] <- NA
  # }

  #Create columns with data_source
  data$data_source <- data_source

  if(!is.null(numeric_columns)){

  if(verbose)
    message("Fixing column classes...")
  #Convert columns to character and numeric
  # #Numeric columns
  # numeric_columns <- c("decimalLongitude", "decimalLatitude",
  #          "coordinateUncertaintyInMeters", "elevation", "year")
  #Character columns
  c_c <- setdiff(colnames(data), numeric_columns)
  data[numeric_columns] <- lapply(data[numeric_columns], function(x) as.numeric(x))
  data[c_c] <- lapply(data[c_c], function(x) as.character(x)) }

  #Fix encoding
  if(!is.null(check_encoding)){

  # #Fix UTF-8 encoding in character columns
  # check_columns <- c("collectionCode", "catalogNumber",
  #                    "country", "stateProvince", "municipality",
  #                    "locality", "eventDate", "recordedBy", "identifiedBy",
  #                    "basisOfRecord", "datasetName")
  for (column in check_encoding){
    if(verbose)
      message("Checking encoding of ", column)
    #Check if is valid
    cc_is_valid <- validUTF8(data[[column]])

    #Get values that are not valid
    to_fix_cc <- unique(data[[column]][!cc_is_valid])

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

  data[[column]][ind_to_fix_cc] %>% unique()

  } #End of check encoding
  }
  #Append data_source
  data$data_source = data_source

  return(data[,c(colnames(metadata), "data_source")])}
