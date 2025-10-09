import_gbif <- function(request_key,
                        output_dir = NULL,
                        write_file = FALSE,
                        file.format = "gz",
                        select_columns = TRUE,
                        columns_to_import = NULL,
                        overwrite = FALSE,
                        ...){ #Other arguments passed to occ_download_import
  # Check if file exists
  if(write_file){
    file_to_save <- paste0(output_dir, "/gbif_occ.", file.format)
    # Check if file exists
    if(file.exists(file_to_save) && !overwrite){
      stop("The file '", file_to_save, "' already exists. Delete the file or set 'overwrite = TRUE'")
    }}

  # Download records to temporary location
  temporary_file <- rgbif::occ_download_get(key = request_key,
                                            path = normalizePath(tempdir()),
                                            overwrite = overwrite)
  # Check columns to import
  if(select_columns){
    if(is.null(columns_to_import)){
      c_imp <- RuHere::gbif_columns
    } else {
      c_imp <- columns_to_import
    }
  } else {
    c_imp <- NULL
  }

  # Import occurrences
  occ <- rgbif::occ_download_import(x = temporary_file, select = c_imp, ...)

  if(write_file){
  data.table::fwrite(occ,
                     file = file_to_save)
  }
  return(occ)
}

# # # Test
# # devtools::load_all()
# request_key <- readRDS("../RuHere_test/req_gbif.rds")
# output_dir = "../RuHere_test/"
# write_file = TRUE
# file.format = "gz"
# select_columns = TRUE
# columns_to_import = NULL
# overwrite = FALSE
# ... = NULL
#
# occ_gbif <- import_gbif(request_key = request_key, write_file = TRUE,
#                         output_dir = "../RuHere_test/", file.format = "gz",
#                         select_columns = FALSE, overwrite = FALSE)
#
#
# # # Preselect columns
# gbif_columns <- c("scientificName", "occurrenceID", "collectionCode",
#                   "catalogNumber","decimalLongitude", "decimalLatitude",
#                   "coordinateUncertaintyInMeters", "elevation",
#                   "continent", "countryCode","stateProvince", "municipality",
#                   "locality", "verbatimLocality", "year", "eventDate",
#                   "recordedBy", "identifiedBy",
#                   "basisOfRecord",
#                   "occurrenceRemarks", "habitat", "datasetName",
#                   "datasetKey")
# usethis::use_data(gbif_columns, overwrite = TRUE)
